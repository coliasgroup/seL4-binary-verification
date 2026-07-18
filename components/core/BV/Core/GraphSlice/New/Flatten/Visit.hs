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
import Data.Maybe (catMaybes)
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
    (const (NormalizedVisit <$> pruneVisit visit p))
    (checkGenerality visit p)
  where
    tagOk = case visit.nodeId of
        Addr addr -> p.analysis.nodeTag addr == visit.tag
        _ -> True

normalizePredVisits
    :: Tag t
    => NormalizedVisit t
    -> M t [NormalizedVisit t]
normalizePredVisits norm p =
    map NormalizedVisit $
        catMaybes $
            map (\v -> pruneVisit v p) predVis
  where
    visit = norm.unwrap
    preds = (viewAtTag visit.tag p.analysis.preds) visit.nodeId
    naivePredVisits = predVisits visit (toList preds)
    predVis = concatMap f naivePredVisits
    f v = either (\split -> splitVisitAt split v) (const [v]) (checkGenerality v p)

pruneVisit :: Tag t => Visit t -> M t (Maybe (Visit t))
pruneVisit visit p =
    forOf #restrs visit $ mapFilterWithKeyA $ \addr vc ->
        if p.analysis.nodeTag addr /= visit.tag
        then return False
        else do
            let reachable = isNonTriviallyReachableFromImpl addr visit.nodeId p
            guard $ reachable || hasZeroVC vc
            return reachable

-- TODO choose one
isNonTriviallyReachableFromImpl :: NodeAddr -> NodeId -> M t Bool
isNonTriviallyReachableFromImpl from to_ p =
--     if x /= y then error (show (Addr from == to_)) else x
--   where
--     x =
        isNonTriviallyReachableFrom p from to_
    -- y =
    --     p.analysis.isNonTriviallyReachableFrom from to_

checkGenerality :: Visit t -> M t (Either NodeAddr ())
checkGenerality visit p =
    for_ (preview #_Addr visit.nodeId) $ \nodeAddr ->
        for_ (outermostLoopContaining p.analysis.loopData nodeAddr) $ \loop ->
            ifor_ visit.restrs $ \addr vc -> do
                let loopOpt' = outermostLoopContaining p.analysis.loopData addr
                when (fmap (.head) loopOpt' == Just loop.head && isOptionsVC vc) $ do
                    Left addr

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
    | otherwise -> VisitKindNormal $ normalizePredVisits norm p

  where
    visit = norm.unwrap
    isEntryPoint = visit.nodeId == (viewAtTag visit.tag p.problem.sides).entryPoint
    isPostLoop = or
        [ Addr addr == visit.nodeId && vc == offsetVC 0
        | (addr, vc) <- M.toList visit.restrs
        ]
