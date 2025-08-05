module BV.Search.Core.Inlining
    ( DiscoverInlineScriptInput (..)
    , discoverInlineScript
    ) where

import BV.Core.Graph
import BV.Core.RepGraph
import BV.Core.Stages
import BV.Core.Structs
import BV.Core.Types
import BV.Core.Types.Extras.Expr
import BV.Core.Types.Extras.Program
import BV.Core.Types.Extras.ProofCheck
import BV.Search.Core.Solver
import BV.Utils (expecting, unwrapped)

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, gets, put)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (execWriterT, tell)
import Data.Foldable (for_, toList)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics

data DiscoverInlineScriptInput
  = DiscoverInlineScriptInput
      { structs :: ByTag' (Map Ident Struct)
      , rodata :: ROData
      , functions :: Ident -> Function
      , matches :: S.Set PairingId'
      , pairingId :: PairingId'
      }
  deriving (Generic)

discoverInlineScript
    :: (Monad m, MonadRepGraphSolverInteract n)
    => (forall a. n a -> m a)
    -> DiscoverInlineScriptInput
    -> m InlineScript'
discoverInlineScript run input =
    evalStateT
        (buildInlineScript composeInliners lookupFun funs)
        [inlineCompletelyUnmatched, inlineReachableUnmatchedC]
  where
    lookupFun = input.functions
    funs = withTags input.pairingId <&> \nameWithTag -> Named nameWithTag.value (lookupFun nameWithTag.value)
    allMatched = S.fromList $ input.matches ^.. folded % folded
    asmToCMatch = M.fromList $ [ (getAsm match, getC match) | match <- S.toList input.matches ]
    presentInProblem problem = S.fromList $ problem ^.. #nodes % folded % #_NodeCall % #functionName
    inlineCompletelyUnmatched problem =
        let matched = S.intersection (presentInProblem problem) allMatched
         in return $ nextCompletelyUnmatchedInlinePoints matched problem
    inlineReachableUnmatchedC problem =
        let matchedC =
                let present = presentInProblem problem
                in S.fromList $ toList $ M.restrictKeys asmToCMatch present
         in fmap (:[]) <$> run (nextReachableUnmatchedCInlinePoint matchedC (RepGraphBaseInput
                { structs = input.structs
                , rodata = input.rodata
                , problem
                }))

type Inliner t m = Problem t -> m (Maybe [NodeAddr])

buildInlineScript :: forall t m. (Tag t, Monad m) => Inliner t m -> (Ident -> Function) -> ByTag t (Named Function) -> m (InlineScript t)
buildInlineScript inliner lookupFun funs = flip evalStateT initProblemBuilder $ do
    addEntrypoints funs
    doAnalysis
    let go = do
            p <- lift $ gets extractProblem
            addrsOpt <- lift $ lift $ inliner p
            for_ addrsOpt $ \addrs -> do
                entries <- lift $ for addrs $ \addr -> inlineAtPoint lookupFun addr <* doAnalysis
                tell entries
                go
    execWriterT go

composeInliners :: Monad m => Inliner t (StateT [Inliner t m] m)
composeInliners problem = go
  where
    go = get >>= \case
        [] -> return Nothing
        x:xs -> lift (x problem) >>= \case
            Just y -> return (Just y)
            Nothing -> put xs >> go

nextCompletelyUnmatchedInlinePoints :: S.Set Ident -> Problem' -> Maybe [NodeAddr]
nextCompletelyUnmatchedInlinePoints matched p = case M.keys (M.filter f p.nodes) of
    [] -> Nothing
    addrs -> Just addrs
  where
    f = \case
        NodeCall callNode -> S.notMember callNode.functionName matched
        _ -> False

nextReachableUnmatchedCInlinePoint :: MonadRepGraphSolverInteract m => S.Set Ident -> RepGraphBaseInput AsmRefineTag -> m (Maybe NodeAddr)
nextReachableUnmatchedCInlinePoint matchedC repGraphInput =
    preview (_Left % #nodeAddr)
        <$> runInlineM repGraphInput inlinerInput nextReachableUnmatchedCInlinePointInner
  where
    inlinerInput = InlinerInput
        { matchedC
        }

nextReachableUnmatchedCInlinePointInner :: MonadRepGraphSolverInteract m => InlineM m ()
nextReachableUnmatchedCInlinePointInner = runForTag C $ do
    p <- askProblem
    g <- askNodeGraph
    heads <- loopHeadsIncludingInner p.nodes <$> askLoopDataMap
    let limits = [ Restr n (doubleRangeVC 3 3) | n <- heads ]
    let reachable = reachableFrom g ((getC p.sides).entryPoint)
    for_ (sortOn compatKey reachable) $ \n -> runExceptT $ tryGetNodePcEnv $ Visit n limits
  where
    -- HACK match graph-refine
    compatKey :: NodeId -> (Int, Maybe NodeAddr)
    compatKey = \case
        Addr n -> (0, Just n)
        Ret -> (1, Nothing)
        Err -> (2, Nothing)

type InlineMInner m = ExceptT InliningEvent (RepGraphBase AsmRefineTag (ReaderT InlinerInput m))

newtype InlineM m a
  = InlineM { run :: InlineMInner m a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadRepGraphSolver
    , MonadRepGraphSolverInteract
    , MonadRepGraphSolverSend
    , MonadStructs
    )

runInlineM :: MonadRepGraphSolverInteract m => RepGraphBaseInput AsmRefineTag -> InlinerInput -> InlineM m a -> m (Either InliningEvent a)
runInlineM repGraphInput inlinerInput m =
    runReaderT (runRepGraphBase repGraphInput (runExceptT m.run)) inlinerInput

data InliningEvent
  = InliningEvent
      { nodeAddr :: NodeAddr
      }
  deriving (Generic)

data InlinerInput
  = InlinerInput
      { matchedC :: S.Set Ident
      }
  deriving (Generic)

instance MonadRepGraphSolverInteract m => MonadRepGraphDefaultHelper AsmRefineTag (InlineMInner m) (InlineM m) where
    liftMonadRepGraphDefaultHelper = InlineM

instance MonadRepGraphSolverInteract m => MonadRepGraph AsmRefineTag (InlineM m) where
    runPreEmitCallNodeHook visit pc env = do
        tag <- askTag
        p <- askProblem
        let nodeAddr = nodeAddrFromNodeId visit.nodeId
        let fname = p ^. #nodes % at nodeAddr % unwrapped % expecting #_NodeCall % #functionName
        matchedC <- lift $ InlineM $ gview #matchedC
        when (tag == C && S.notMember fname matchedC) $ do
            hyp <- withEnv env $ convertExprNoSplit $ notE pc
            res <- testHyp hyp
            unless res $ lift $ InlineM $ throwError $ InliningEvent
                { nodeAddr
                }
