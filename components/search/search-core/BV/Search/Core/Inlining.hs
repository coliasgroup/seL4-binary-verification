{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Search.Core.Inlining
    ( DiscoverInlineScriptInput (..)
    , discoverInlineScript
    ) where

import BV.Search.Core.GraphSlice
import BV.Search.Core.Solver

import BV.Core.Stages
import BV.Core.Types
import BV.Core.Types.Extras.Expr (notE)
import BV.Core.Types.Extras.Problem
import BV.Core.Types.Extras.Program (nodeAddrOf)
import BV.Core.Types.Extras.ProofCheck
import BV.Utils (expectingAt, is)

import Control.Applicative (asum)
import Control.Monad (filterM, guard, unless)
import Control.Monad.Extra (whenM)
import Control.Monad.State (StateT, evalStateT, get, gets, put)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Writer (execWriterT, tell)
import Data.Foldable (for_, toList, traverse_)
import Data.Foldable.Extra (anyM)
import Data.Functor (void)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
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
    allMatched = S.fromList $ input.matches ^.. folded % to withTags % folded
    asmToCMatch = M.fromList $ [ (WithTag Asm match.asm, WithTag C match.c) | match <- S.toList input.matches ]
    presentInProblem pwa = S.fromList $ flip foldMap (withTags pwa.problem.sides) $ \(WithTag tag side) -> side ^.. #nodes % folded % #_NodeCall % #functionName % to (WithTag tag)
    inlineCompletelyUnmatched pwa =
        let matched = S.intersection (presentInProblem pwa) allMatched
         in return $ nextCompletelyUnmatchedInlinePoints matched pwa.problem
    inlineReachableUnmatchedC pwa =
        let matchedC =
                let present = presentInProblem pwa
                in S.fromList $ toList $ M.restrictKeys asmToCMatch present
         in run $ nextReachableUnmatchedCInlinePoints matchedC $ GraphSliceInput
                { structs = input.structs
                , rodata = input.rodata
                , pwa
                }

type Inliner t m = ProblemWithAnalysis t -> m [WithTag t NodeAddr]

buildInlineScript :: forall t m. (Tag t, Monad m) => Inliner t m -> (WithTag t Ident -> Function) -> ByTag t (Named Function) -> m (InlineScript t)
buildInlineScript inliner lookupFun funs = flip evalStateT initProblemBuilder $ do
    addEntrypoints funs
    doAnalysis
    let go = do
            p <- lift $ gets extractProblemWithAnalysis
            addrs <- lift $ lift $ inliner p
            unless (null addrs) $ do
                entries <- lift $ traverse inlineEntryForPoint addrs
                lift $ traverse (inline lookupFun) entries
                lift $ doAnalysis
                tell entries
                go
    execWriterT go

composeInliners :: Monad m => Inliner t (StateT [Inliner t m] m)
composeInliners pwa = go
  where
    go = get >>= \case
        [] -> return []
        x:xs -> lift (x pwa) >>= \case
            [] -> put xs >> go
            ys -> return ys

nextCompletelyUnmatchedInlinePoints :: S.Set (WithTag AsmRefineTag Ident) -> Problem' -> [WithTag AsmRefineTag NodeAddr]
nextCompletelyUnmatchedInlinePoints matched p = foldMap g (withTags p.sides)
  where
    g (WithTag tag side) = map (WithTag tag) $ M.keys (M.filter (f tag) side.nodes)
    f tag = \case
        NodeCall callNode -> S.notMember (WithTag tag callNode.functionName) matched
        _ -> False

nextReachableUnmatchedCInlinePoints
    :: MonadGraphSliceSolverInteract m
    => S.Set (WithTag AsmRefineTag Ident)
    -> GraphSliceInput AsmRefineTag
    -> m [WithTag AsmRefineTag NodeAddr]
nextReachableUnmatchedCInlinePoints matchedC repGraphInput =
    runGraphSliceT defaultGraphSliceHooks repGraphInput $
        nextReachableUnmatchedCInlinePointsInner matchedC

nextReachableUnmatchedCInlinePointsInner
    :: MonadGraphSliceSolverInteract m
    => S.Set (WithTag AsmRefineTag Ident)
    -> GraphSliceT AsmRefineTag m [WithTag AsmRefineTag NodeAddr]
nextReachableUnmatchedCInlinePointsInner =
    nextReachableUnmatchedCInlinePointsInnerIncompat
    -- nextReachableUnmatchedCInlinePointsInnerCompat

nextReachableUnmatchedCInlinePointsInnerIncompat
    :: MonadGraphSliceSolverInteract m
    => S.Set (WithTag AsmRefineTag Ident)
    -> GraphSliceT AsmRefineTag m [WithTag AsmRefineTag NodeAddr]
nextReachableUnmatchedCInlinePointsInnerIncompat matchedC = do
    side <- problemSideWithAnalysis C <$> askProblemWithAnalysis
    let g = side.analysis.nodeGraph
    let loops = allLoopsOf $ side.analysis.loopData
    let limits = M.fromList [ (loop.head, doubleRangeVC 3 3) | loop <- loops ]
    for_ (reachableFrom g side.problem.entryPoint) $ \n -> do
        let visit = Visit C n limits
        whenM (isVisitOk visit) $ void $ getPcEnv visit
    visitsByFunName <- getCallsByFunName
    let unmatchedByAddr = foldl (M.unionWith S.union) M.empty
            [ M.singleton (WithTag visit.tag (nodeAddrOf visit.nodeId)) (S.singleton visit)
            | (WithTag C funName, visits) <- M.toList visitsByFunName
            , S.notMember (WithTag C funName) matchedC
            , visit <- visits
            ]
    fmap (map fst) $ flip filterM (M.toList unmatchedByAddr) $ \(_addr, visits) ->
        flip anyM visits $ \visit -> do
            pc <- getPc visit
            unreachable <- testHyp $ notE pc
            return $ not unreachable

nextReachableUnmatchedCInlinePointsInnerCompat
    :: MonadGraphSliceSolverInteract m
    => S.Set Ident
    -> GraphSliceT AsmRefineTag m [NodeAddr]
nextReachableUnmatchedCInlinePointsInnerCompat matchedC = do
    side <- problemSideWithAnalysis C <$> askProblemWithAnalysis
    let g = side.analysis.nodeGraph
    let loops = allLoopsOf $ side.analysis.loopData
    let limits = M.fromList [ (loop.head, doubleRangeVC 3 3) | loop <- loops ]
    let reachable = reachableFrom g side.problem.entryPoint
    let f n = do
            let visit = Visit C n limits
            whenM (isVisitOk visit) $ void $ getPcEnv visit
    -- HACK order matches graph-refine
    traverse_ f $ sort $ filter (is #_Addr) reachable
    f Ret
    f Err
    funCallVisits <- getCallOrderCompat
    -- HACK return just one result at a time to match graph-refine
    fmap toList $ runMaybeT $ asum $ flip map funCallVisits $ \visit -> do
        let Addr addr = visit.nodeId
        let Just fname = side.problem.nodes ^? expectingAt addr % #_NodeCall % #functionName
        guard $ visit.tag == C
        guard $ S.notMember fname matchedC
        res <- lift $ do
            pcEnv <- fromJust <$> getPcEnv visit
            testHyp $ notE pcEnv.pc
        guard $ not res
        return addr
