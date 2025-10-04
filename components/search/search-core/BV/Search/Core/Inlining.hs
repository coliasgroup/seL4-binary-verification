module BV.Search.Core.Inlining
    ( DiscoverInlineScriptInput (..)
    , discoverInlineScript
    ) where

import BV.Core.GraphSlice.New
import BV.Core.Stages
import BV.Core.Types
import BV.Core.Types.Extras.Expr (notE)
import BV.Core.Types.Extras.Problem
import BV.Core.Types.Extras.Program
import BV.Core.Types.Extras.ProofCheck
import BV.Search.Core.Solver
import BV.Utils (expecting, expectingAt, is)

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, evalStateT, get, gets, put)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (execWriterT, tell)
import Data.Foldable (for_, toList, traverse_)
import Data.Functor (void)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics

data DiscoverInlineScriptInput
  = DiscoverInlineScriptInput
      { structs :: ByTag' (Map Ident Struct)
      , rodata :: ROData
      , lookupFunction :: WithTag' Ident -> Function
      , matches :: S.Set PairingId'
      , pairingId :: PairingId'
      }
  deriving (Generic)

discoverInlineScript
    :: (Monad m, MonadGraphSliceSolverInteract n)
    => (forall a. n a -> m a)
    -> DiscoverInlineScriptInput
    -> m InlineScript'
discoverInlineScript run input =
    evalStateT
        (buildInlineScript composeInliners lookupFun funs)
        [inlineCompletelyUnmatched, inlineReachableUnmatchedC]
  where
    lookupFun = input.lookupFunction
    funs = withTags input.pairingId <&> \nameWithTag -> Named nameWithTag.value (lookupFun nameWithTag)
    allMatched = S.fromList $ input.matches ^.. folded % folded
    asmToCMatch = M.fromList $ [ (match.asm, match.c) | match <- S.toList input.matches ]
    presentInProblem problem = S.fromList $ problem ^.. #nodes % folded % #_NodeCall % #functionName
    inlineCompletelyUnmatched problem =
        let matched = S.intersection (presentInProblem problem) allMatched
         in return $ nextCompletelyUnmatchedInlinePoints matched problem
    inlineReachableUnmatchedC problem =
        let matchedC =
                let present = presentInProblem problem
                in S.fromList $ toList $ M.restrictKeys asmToCMatch present
         in fmap (:[]) <$> run (nextReachableUnmatchedCInlinePoint matchedC (GraphSliceInput
                { structs = input.structs
                , rodata = input.rodata
                , problem
                }))

type Inliner t m = Problem t -> m (Maybe [NodeAddr])

buildInlineScript :: forall t m. (Tag t, Monad m) => Inliner t m -> (WithTag t Ident -> Function) -> ByTag t (Named Function) -> m (InlineScript t)
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

nextReachableUnmatchedCInlinePoint :: forall m. MonadGraphSliceSolverInteract m => S.Set Ident -> GraphSliceInput AsmRefineTag -> m (Maybe NodeAddr)
nextReachableUnmatchedCInlinePoint matchedC repGraphInput =
    preview (_Left % #nodeAddr)
        <$> runExceptT (runGraphSliceT hooks repGraphInput nextReachableUnmatchedCInlinePointInner)
  where
    hooks = (defaultGraphSliceHooks :: GraphSliceHooks AsmRefineTag m) & #preEmitCallNode .~ inlinerHook
    inlinerHook visit = do
        tag <- askTag
        p <- askProblem
        let nodeAddr = nodeAddrOf visit.nodeId
        let fname = p ^. #nodes % expectingAt nodeAddr % expecting #_NodeCall % #functionName
        when (tag == C && S.notMember fname matchedC) $ do
            pcEnv <- fromJust <$> getNodePcEnv visit
            hyp <- liftUntagged $ convertExpr $ notE pcEnv.pc
            res <- liftUntagged $ testHyp hyp
            unless res $ liftUntagged $ lift $ throwError $ InliningEvent
                { nodeAddr
                }

nextReachableUnmatchedCInlinePointInner :: MonadGraphSliceSolverInteract m => GraphSliceT AsmRefineTag (ExceptT InliningEvent m) ()
nextReachableUnmatchedCInlinePointInner = runTagged C $ do
    p <- askProblem
    g <- askNodeGraph
    loops <- allInnerLoops p.nodes <$> askLoopData
    let limits = [ Restr loop.head (doubleRangeVC 3 3) | loop <- loops ]
    let reachable = reachableFrom g p.sides.c.entryPoint
    let f n = void $ tryGetNodePcEnv $ Visit n limits
    -- HACK order matches graph-refine
    traverse_ f $ sort $ filter (is #_Addr) reachable
    f Ret
    f Err

data InliningEvent
  = InliningEvent
      { nodeAddr :: NodeAddr
      }
  deriving (Generic)
