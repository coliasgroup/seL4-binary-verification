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
import BV.Core.Types.Extras.ProofCheck
import BV.Utils (setFilterA)

import Control.Monad (unless)
import Control.Monad.State (StateT, evalState, evalStateT, get, gets, modify,
                            put)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (execWriterT, tell)
import Data.Foldable (toList)
import Data.Foldable.Extra (anyM)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust)
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
    flip evalStateT S.empty $
    flip evalStateT [inlineCompletelyUnmatched, inlineReachableUnmatchedC] $
        (buildInlineScript composeInliners lookupFun funs)
  where
    lookupFun = input.lookupFunction
    funs = withTags input.pairingId <&> \name -> Named name.value (lookupFun name)
    inlineCompletelyUnmatched builder =
        let pwa = extractProblemWithAnalysis builder
            matchExists = flip S.member $ S.fromList $ input.matches ^.. folded % to withTags % folded
         in return
                [ WithTag tag addr
                | WithTag tag side <- toList $ withTags pwa.problem.sides
                , (addr, NodeCall callNode) <- M.toList side.nodes
                , not $ matchExists $ WithTag tag callNode.functionName
                ]
    inlineReachableUnmatchedC builder =
        let pwa = extractProblemWithAnalysis builder
            present = presentInProblem pwa
            matchPresentInProblem = S.fromList
                [ viewAtTag C p
                | p <- toList input.matches
                , viewAtTag Asm $ S.member <$> p <*> present
                ]
            candidatesWithHaveBody =
                [ (WithTag C addr, haveBody)
                | (addr, NodeCall callNode) <- M.toList (viewAtTag C pwa.problem.sides).nodes
                , callNode.functionName `S.notMember` matchPresentInProblem
                , let haveBody = isJust (lookupFun (WithTag C callNode.functionName)).body
                ]
            candidates = map fst candidatesWithHaveBody
            candidatesWithBody = map fst $ filter snd candidatesWithHaveBody
            speculativelyInlined = flip evalState builder $ do
                traverse (inlineAtPoint lookupFun) candidatesWithBody
                doAnalysis
                gets extractProblemWithAnalysis
            filterInput = GraphSliceInput
                { structs = input.structs
                , rodata = input.rodata
                , pwa = speculativelyInlined
                }
         in S.toList <$> filterLive run filterInput (S.fromList candidates)

presentInProblem :: Tag t => ProblemWithAnalysis t -> ByTag t (S.Set Ident)
presentInProblem pwa = pwa.problem.sides <&>
    S.fromList . toListOf (#nodes % folded % #_NodeCall % #functionName)

type Inliner t m = ProblemBuilder t -> m [WithTag t NodeAddr]

buildInlineScript :: forall t m. (Tag t, Monad m) => Inliner t m -> (WithTag t Ident -> Function) -> ByTag t (Named Function) -> m (InlineScript t)
buildInlineScript inliner lookupFun funs = flip evalStateT initProblemBuilder $ do
    addEntrypoints funs
    doAnalysis
    let go = do
            builder <- lift $ get
            addrs <- lift $ lift $ inliner builder
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

filterLive
    :: (Monad m, MonadGraphSliceSolverInteract n)
    => (forall a. n a -> m a)
    -> GraphSliceInput AsmRefineTag
    -> S.Set (WithTag AsmRefineTag NodeAddr)
    -> StateT (S.Set (WithTag AsmRefineTag NodeAddr)) m (S.Set (WithTag AsmRefineTag NodeAddr))
filterLive run input candidates = do
    dead <- get
    let liveCandidates = candidates `S.difference` dead
    live <- lift $ run $ runGraphSliceT defaultGraphSliceHooks input $ do
        swa <- problemSideWithAnalysis C <$> askProblemWithAnalysis
        let limits = M.fromList
                [ (loop.head, doubleRangeVC 3 3)
                | loop <- allLoopsOf swa.analysis.loopData
                ]
        flip setFilterA liveCandidates $ \(WithTag _ addr) -> do
            visits <- splitVisit $ Visit C (Addr addr) limits
            flip anyM visits $ \visit -> do
                pc <- getPc visit
                unreachable <- testHyp $ notE pc
                return $ not unreachable
    modify $ S.union $ liveCandidates `S.difference` live
    return live
