{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Types.Extras.ProofCheck
    ( SimpleVisitCountView (..)
    , contVisits
    , doubleRangeVC
    , enumerateSimpleVCs
    , eqH
    , eqIfAtH
    , eqIfAtInductH
    , eqInductByTagH
    , eqInductH
    , eqInductSingleH
    , eqSideH
    , eqWithIfAtH
    , fromRestrKindVC
    , fromSimpleVC
    , hasZeroVC
    , incrVC
    , isEmptyVC
    , isOptionsVC
    , numberVC
    , numbersVC
    , offsetVC
    , offsetsVC
    , pcFalseH
    , pcImpH
    , pcTrivH
    , pcTrueH
    , predVisits
    , splitVisitAt
    , toSimpleVC
    , trueIfAt
    , upToVC
    , visitFromCompat
    , visitToCompat
    ) where

import BV.Core.Types
import BV.Core.Types.Extras.Expr
import BV.Core.Types.Extras.Program (nodeAddrOf)
import BV.Utils (ensure)

import Control.DeepSeq (NFData)
import Control.Monad (guard)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import GHC.Generics (Generic)
import Optics

numberVC :: Integer -> VisitCount
numberVC n = numbersVC [n]

numbersVC :: [Integer] -> VisitCount
numbersVC numbers = VisitCount
    { numbers
    , offsets = []
    }

offsetVC :: Integer -> VisitCount
offsetVC n = offsetsVC [n]

offsetsVC :: [Integer] -> VisitCount
offsetsVC offsets = VisitCount
    { numbers = []
    , offsets
    }

isOptionsVC :: VisitCount -> Bool
isOptionsVC vc = length vc.numbers + length vc.offsets > 1

isEmptyVC :: VisitCount -> Bool
isEmptyVC = (==) mempty

data SimpleVisitCountView
  = SimpleVisitCountViewNumber Integer
  | SimpleVisitCountViewOffset Integer
  deriving (Eq, Generic, NFData, Ord, Show)

toSimpleVC :: VisitCount -> Maybe SimpleVisitCountView
toSimpleVC = \case
    VisitCount { numbers = [n], offsets = [] } -> Just (SimpleVisitCountViewNumber n)
    VisitCount { numbers = [], offsets = [n] } -> Just (SimpleVisitCountViewOffset n)
    _ -> Nothing

enumerateSimpleVCs :: VisitCount -> [SimpleVisitCountView]
enumerateSimpleVCs vc = map SimpleVisitCountViewNumber vc.numbers ++ map SimpleVisitCountViewOffset vc.offsets

fromSimpleVC :: SimpleVisitCountView -> VisitCount
fromSimpleVC = \case
    SimpleVisitCountViewNumber n -> numberVC n
    SimpleVisitCountViewOffset n -> offsetVC n

fromRestrKindVC :: RestrProofNodeRangeKind -> Integer -> VisitCount
fromRestrKindVC kind n = n & case kind of
    RestrProofNodeRangeKindNumber -> numberVC
    RestrProofNodeRangeKindOffset -> offsetVC

upToVC :: Integer -> VisitCount
upToVC n = foldMap numberVC [0 .. n - 1]

doubleRangeVC :: Integer -> Integer -> VisitCount
doubleRangeVC n m = foldMap numberVC [0 .. n - 1] <> foldMap offsetVC [0 .. m - 1]

hasZeroVC :: VisitCount -> Bool
hasZeroVC vc = 0 `elem` vc.numbers

incrVC :: Integer -> VisitCount -> VisitCount
incrVC incr = (#numbers %~ f) . (#offsets %~ f)
  where
    f = filter (>= 0) . map (+ incr)

splitVisitAt :: NodeAddr -> Visit -> [Visit]
splitVisitAt split visit =
    enumerateSimpleVCs (visit.restrs M.! split) <&> \(SimpleVisitCountViewOffset i) ->
        visit & #restrs %~ M.insert split (offsetVC i)

predVisits :: Visit -> [NodeAddr] -> [Visit]
predVisits visit = mapMaybe f
  where
    f pred_ = Visit (Addr pred_) <$> incrVCs (-1) pred_ visit.restrs

contVisits :: Visit -> [NodeId] -> [Visit]
contVisits visit conts =
    [ Visit
        { nodeId = cont
        , restrs = fromJust $ incrVCs 1 (nodeAddrOf visit.nodeId) visit.restrs
        }
    | cont <- conts
    ]

incrVCs :: Integer -> NodeAddr -> M.Map NodeAddr VisitCount -> Maybe (M.Map NodeAddr VisitCount)
incrVCs incr = M.alterF $ \case
    Nothing -> return Nothing
    Just vc ->
        let vc' = incrVC incr vc
         in do guard $ isEmptyVC vc'
               return (Just vc')

visitFromCompat :: VisitCompat -> Visit
visitFromCompat compat = Visit
    { nodeId = compat.nodeId
    , restrs = M.fromListWith (error "repeat")
        [ (restr.nodeAddr, restr.visitCount)
        | restr <- compat.restrs
        ]
    }

visitToCompat :: Visit -> VisitCompat
visitToCompat visit = VisitCompat
    { nodeId = visit.nodeId
    , restrs =
        [ Restr addr vc
        | (addr, vc) <- M.toList visit.restrs
        ]
    }

--

eqH :: EqHypSide t -> EqHypSide t -> Hyp t
eqH lhs rhs = HypEq
    { ifAt = False
    , eq = EqHyp
        { lhs
        , rhs
        , induct = Nothing
        }
    }

eqInductH :: EqHypInduct -> EqHypSide t -> EqHypSide t -> Hyp t
eqInductH induct lhs rhs = HypEq
    { ifAt = False
    , eq = EqHyp
        { lhs
        , rhs
        , induct = Just induct
        }
    }

eqIfAtInductH :: EqHypInduct -> EqHypSide t -> EqHypSide t -> Hyp t
eqIfAtInductH induct lhs rhs = HypEq
    { ifAt = True
    , eq = EqHyp
        { lhs
        , rhs
        , induct = Just induct
        }
    }

eqIfAtH :: EqHypSide t -> EqHypSide t -> Hyp t
eqIfAtH lhs rhs = HypEq
    { ifAt = True
    , eq = EqHyp
        { lhs
        , rhs
        , induct = Nothing
        }
    }

eqWithIfAtH :: Bool -> EqHypSide t -> EqHypSide t -> Maybe EqHypInduct -> Hyp t
eqWithIfAtH ifAt lhs rhs induct = HypEq
    { ifAt
    , eq = EqHyp
        { lhs
        , rhs
        , induct
        }
    }

trueIfAt :: GraphExpr -> WithTag t VisitCompat -> Hyp t
trueIfAt expr visit = eqIfAtH (eqSideH expr visit) (eqSideH trueE visit)

pcTrueH :: WithTag t VisitCompat -> Hyp t
pcTrueH visit = HypPcImp (PcImpHyp
    { lhs = PcImpHypSideBool True
    , rhs = PcImpHypSidePc visit
    })

pcFalseH :: WithTag t VisitCompat -> Hyp t
pcFalseH visit = HypPcImp (PcImpHyp
    { lhs = PcImpHypSidePc visit
    , rhs = PcImpHypSideBool False
    })

pcTrivH :: WithTag t VisitCompat -> Hyp t
pcTrivH visit = HypPcImp (PcImpHyp
    { lhs = PcImpHypSidePc visit
    , rhs = PcImpHypSidePc visit
    })

pcImpH :: PcImpHypSide t -> PcImpHypSide t -> Hyp t
pcImpH lhs rhs = HypPcImp (PcImpHyp { lhs, rhs })

eqSideH :: GraphExpr -> WithTag t VisitCompat -> EqHypSide t
eqSideH = EqHypSide

-- HACK integer representation matches graph-refine

eqInductByTagH :: RefineTag t => ByTag t NodeAddr -> EqHypInduct
eqInductByTagH addrs = ensure (0 `notElem` rawAddrs) $ withByRefineTag EqHypInduct rawAddrs
  where
    rawAddrs = (.unwrap) <$> addrs

eqInductSingleH :: NodeAddr -> EqHypInduct
eqInductSingleH addr = EqHypInduct addr.unwrap 0
