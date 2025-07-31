module BV.Search.Inlining
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
import BV.Search.Solver
import BV.Search.Utils

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError, tryError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (lift)
import Data.Foldable (for_, toList)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

data DiscoverInlineScriptInput
  = DiscoverInlineScriptInput
      { structs :: ByTag' (Map Ident Struct)
      , rodata :: ROData
      , functions :: WithTag' Ident -> Function
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
    funs = withTags input.pairingId <&> \nameWithTag -> Named nameWithTag.value (lookupFun nameWithTag)
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
nextReachableUnmatchedCInlinePointInner = InlineM $ do
    p <- askProblem
    heads <- loopHeadsIncludingInner p.nodes <$> askLoopDataMap
    let limits = [ Restr n (doubleRangeVC 3 3) | n <- heads ]
    for_ (M.keys p.nodes) $ \n -> tryError $ getNodePcEnv (Visit (Addr n) limits) Nothing
    getNodePcEnv (Visit Ret limits) (Just C)
    getNodePcEnv (Visit Err limits) (Just C)
    return ()

type InlineMInner m = ExceptT InliningEvent (RepGraphBase AsmRefineTag (ReaderT InlinerInput m))

newtype InlineM m a
  = InlineM { run :: InlineMInner m a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadRepGraphSolver
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
    runPreEmitCallNodeHook visit pc env = InlineM $ do
        let nodeAddr = nodeAddrFromNodeId visit.nodeId
        p <- askProblem
        tag <- askNodeTag nodeAddr
        let fname = p ^. #nodes % at nodeAddr % unwrapped % expecting #_NodeCall % #functionName
        matchedC <- gview #matchedC
        when (tag == C && S.notMember fname matchedC) $ do
            hyp <- withEnv env $ convertExprNoSplit $ notE pc
            res <- checkHyp hyp
            unless res $ throwError $ InliningEvent
                { nodeAddr
                }
