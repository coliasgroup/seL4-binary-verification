{-# LANGUAGE MultiWayIf #-}

module BV.Core.GraphSlice.New.Flatten.Visit
    ( NormalizedVisit
    , VisitKind (..)
    , VisitTooGeneral (..)
    , normalizeVisit
    , unwrapNormalizedVisit
    , visitKind
    ) where

import BV.Core.Types
import BV.Core.Types.Extras
import BV.Utils (mapFilterWithKeyA)

import BV.Utils (ensure)
import Control.Monad (guard, when)
import Data.Bifunctor (bimap)
import Data.Foldable (for_, toList)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Optics

type M t = (->) (ProblemWithAnalysis t)

newtype NormalizedVisit t
  = NormalizedVisit { unwrap :: Visit t }
  deriving (Eq, Generic, Ord, Show)

unwrapNormalizedVisit :: NormalizedVisit t -> Visit t
unwrapNormalizedVisit = (.unwrap)

data VisitTooGeneral
  = VisitTooGeneral

normalizeVisit
    :: Tag t
    => Visit t
    -> M t (Either VisitTooGeneral (Maybe (NormalizedVisit t)))
normalizeVisit visit p = ensure tagOk $ bimap
    (const VisitTooGeneral)
    (const (NormalizedVisit <$> pruneVisit side visit))
    (checkGenerality side visit)
  where
    side = problemSideWithAnalysis visit.tag p
    tagOk = case visit.nodeId of
        Addr addr -> addr `M.member` side.problem.nodes
        _ -> True

normalizePredVisits
    :: Tag t
    => ProblemSideWithAnalysis
    -> NormalizedVisit t
    -> [NormalizedVisit t]
normalizePredVisits side norm =
    NormalizedVisit <$> mapMaybe (pruneVisit side) (concatMap expand naivePredVisits)
  where
    visit = norm.unwrap
    predAddrs = side.analysis.preds visit.nodeId
    naivePredVisits = predVisits visit (toList predAddrs)
    expand v = either (\split -> splitVisitAt split v) (const [v]) (checkGenerality side v)

pruneVisit :: Tag t => ProblemSideWithAnalysis -> Visit t -> Maybe (Visit t)
pruneVisit side visit =
    forOf #restrs visit $ mapFilterWithKeyA $ \addr vc ->
        if addr `M.notMember` side.problem.nodes
        then return False
        else do
            let reachable = isNonTriviallyReachableFromImpl side addr visit.nodeId
            guard $ reachable || hasZeroVC vc
            return reachable

-- TODO choose one
isNonTriviallyReachableFromImpl :: ProblemSideWithAnalysis -> NodeAddr -> NodeId -> Bool
isNonTriviallyReachableFromImpl side from to_ =
--     if x /= y then error (show (Addr from == to_)) else x
--   where
--     x =
        isNonTriviallyReachableFrom side from to_
    -- y =
    --     side.analysis.isNonTriviallyReachableFrom from to_

checkGenerality :: ProblemSideWithAnalysis -> Visit t -> Either NodeAddr ()
checkGenerality side visit =
    for_ (preview #_Addr visit.nodeId) $ \visitAddr ->
        for_ (lookupLoop visitAddr) $ \visitLoop ->
            ifor_ visit.restrs $ \addr vc -> do
                let loopOpt' = lookupLoop addr
                when (fmap (.head) loopOpt' == Just visitLoop.head && isOptionsVC vc) $ do
                    Left addr
  where
    lookupLoop = outermostLoopContaining side.analysis.loopData

data VisitKind t
  = VisitKindEntryPoint
  | VisitKindPostLoop
      { preLoop :: NormalizedVisit t
      }
  | VisitKindNormal
      { preds :: [NormalizedVisit t]
      }
  deriving (Eq, Generic, Ord, Show)

visitKind
    :: Tag t
    => NormalizedVisit t
    -> M t (VisitKind t)
visitKind norm p = if
    | isEntryPoint -> VisitKindEntryPoint
    | isPostLoop -> VisitKindPostLoop $
            NormalizedVisit $ visit & #restrs %~ M.insert (nodeAddrOf visit.nodeId) (numberVC 0)
    | otherwise -> VisitKindNormal $ normalizePredVisits side norm

  where
    visit = norm.unwrap
    side = problemSideWithAnalysis visit.tag p
    isEntryPoint = visit.nodeId == (viewAtTag visit.tag p.problem.sides).entryPoint
    isPostLoop = or
        [ Addr addr == visit.nodeId && vc == offsetVC 0
        | (addr, vc) <- M.toList visit.restrs
        ]
