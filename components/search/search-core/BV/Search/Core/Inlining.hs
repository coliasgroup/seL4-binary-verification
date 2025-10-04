{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Search.Core.Inlining
    ( DiscoverInlineScriptInput (..)
    , discoverInlineScript
    ) where

import BV.Search.Core.GraphSlice

import BV.Core.Stages
import BV.Core.Types
import BV.Core.Types.Extras.Expr (notE)
import BV.Core.Types.Extras.Problem
import BV.Core.Types.Extras.ProofCheck
import BV.Search.Core.Solver
import BV.Utils (expectingAt, is)

import Control.Applicative (asum)
import Control.Monad (guard)
import Control.Monad.State (StateT, evalStateT, get, gets, put)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
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
         in run $ nextReachableUnmatchedCInlinePoints matchedC $ GraphSliceInput
                { structs = input.structs
                , rodata = input.rodata
                , problem
                }

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

nextReachableUnmatchedCInlinePoints
    :: MonadGraphSliceSolverInteract m
    => S.Set Ident
    -> GraphSliceInput AsmRefineTag
    -> m (Maybe [NodeAddr])
nextReachableUnmatchedCInlinePoints matchedC repGraphInput =
    runGraphSliceT defaultGraphSliceHooks repGraphInput $
        nextReachableUnmatchedCInlinePointsInner matchedC

nextReachableUnmatchedCInlinePointsInner
    :: MonadGraphSliceSolverInteract m
    => S.Set Ident
    -> GraphSliceT AsmRefineTag m (Maybe [NodeAddr])
nextReachableUnmatchedCInlinePointsInner matchedC = runTagged C $ do
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
    export <- liftUntagged askExport
    -- HACK return just one result at a time to match graph-refine
    runMaybeT $ asum $ flip map (toList export.funCallOrder) $ \(WithTag tag visit) -> do
        guard $ tag == C
        let Addr addr = visit.nodeId
        problem <- lift askProblem
        let Just fname = problem ^? #nodes % expectingAt addr % #_NodeCall % #functionName
        guard $ S.notMember fname matchedC
        pcEnv <- lift $ fromJust <$> getNodePcEnv visit
        hyp <- lift $ liftUntagged $ convertExpr $ notE pcEnv.pc
        res <- lift $ liftUntagged $ testHyp hyp
        guard $ not res
        return [addr]
