{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.Extras.ProofCheck
    ( SimpleVisitCountView (..)
    , asmV
    , cV
    , doubleRangeVC
    , enumerateSimpleVC
    , eqH
    , eqH'
    , eqIfAtH
    , eqIfAtH'
    , eqInductH
    , eqSideH
    , eqWithIfAtH
    , fromRestrKindVC
    , fromSimpleVisitCountView
    , hasZeroVC
    , incrVC
    , isEmptyVC
    , isOptionsVC
    , numberVC
    , offsetVC
    , optionsVC
    , pcFalseH
    , pcImpH
    , pcTrivH
    , pcTrueH
    , simpleVisitCountView
    , tagV
    , trueIfAt
    , trueIfAt'
    , upToVC
    ) where

import BV.Core.Types
import BV.Core.Types.Extras.Expr

import Control.DeepSeq (NFData)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Optics ((%~))

numberVC :: Integer -> VisitCount
numberVC n = VisitCount
    { numbers = [n]
    , offsets = []
    }

offsetVC :: Integer -> VisitCount
offsetVC n = VisitCount
    { numbers = []
    , offsets = [n]
    }

optionsVC :: [VisitCount] -> VisitCount
optionsVC = fold

isOptionsVC :: VisitCount -> Bool
isOptionsVC vc = length vc.numbers + length vc.offsets > 1

isEmptyVC :: VisitCount -> Bool
isEmptyVC = \case
    VisitCount
        { numbers = []
        , offsets = []
        } -> True
    _ -> False

data SimpleVisitCountView
  = SimpleVisitCountViewNumber Integer
  | SimpleVisitCountViewOffset Integer
  deriving (Eq, Generic, NFData, Ord, Show)

simpleVisitCountView :: VisitCount -> Maybe SimpleVisitCountView
simpleVisitCountView = \case
    VisitCount { numbers = [n], offsets = [] } -> Just (SimpleVisitCountViewNumber n)
    VisitCount { numbers = [], offsets = [n] } -> Just (SimpleVisitCountViewOffset n)
    _ -> Nothing

fromSimpleVisitCountView :: SimpleVisitCountView -> VisitCount
fromSimpleVisitCountView = \case
    SimpleVisitCountViewNumber n -> numberVC n
    SimpleVisitCountViewOffset n -> offsetVC n

enumerateSimpleVC :: VisitCount -> [SimpleVisitCountView]
enumerateSimpleVC vc = map SimpleVisitCountViewNumber vc.numbers ++ map SimpleVisitCountViewOffset vc.offsets

fromRestrKindVC :: RestrProofNodeRangeKind -> Integer -> VisitCount
fromRestrKindVC kind n = n & case kind of
    RestrProofNodeRangeKindNumber -> numberVC
    RestrProofNodeRangeKindOffset -> offsetVC

upToVC :: Integer -> VisitCount
upToVC n = optionsVC (map numberVC [0 .. n - 1])

doubleRangeVC :: Integer -> Integer -> VisitCount
doubleRangeVC n m = optionsVC $
    map numberVC [0 ..  n - 1] ++ map offsetVC [0 ..  m - 1]

hasZeroVC :: VisitCount -> Bool
hasZeroVC vc = 0 `elem` vc.numbers

incrVC :: Integer -> VisitCount -> VisitCount
incrVC incr = (#numbers %~ f) . (#offsets %~ f)
  where
    f = filter (>= 0) . map (+ incr)

tagV :: Tag -> Visit -> VisitWithTag
tagV tag visit = VisitWithTag visit tag

cV :: Visit -> VisitWithTag
cV = tagV C

asmV :: Visit -> VisitWithTag
asmV = tagV Asm

--

eqH :: EqHypSide -> EqHypSide -> Maybe EqHypInduct -> Hyp
eqH = eqWithIfAtH False

eqH' :: EqHypSide -> EqHypSide -> Hyp
eqH' lhs rhs = eqH lhs rhs Nothing

eqIfAtH :: EqHypSide -> EqHypSide -> Maybe EqHypInduct -> Hyp
eqIfAtH = eqWithIfAtH True

eqIfAtH' :: EqHypSide -> EqHypSide -> Hyp
eqIfAtH' lhs rhs = eqIfAtH lhs rhs Nothing

eqWithIfAtH :: Bool -> EqHypSide -> EqHypSide -> Maybe EqHypInduct -> Hyp
eqWithIfAtH ifAt lhs rhs induct = HypEq
    { ifAt
    , eq = EqHyp { lhs, rhs, induct }
    }

trueIfAt :: Expr -> VisitWithTag -> Maybe EqHypInduct -> Hyp
trueIfAt expr visit = eqIfAtH (eqSideH expr visit) (eqSideH trueE visit)

trueIfAt' :: Expr -> VisitWithTag -> Hyp
trueIfAt' expr visit = trueIfAt expr visit Nothing

pcTrueH :: VisitWithTag -> Hyp
pcTrueH visit = HypPcImp (PcImpHyp
    { lhs = PcImpHypSideBool True
    , rhs = PcImpHypSidePc visit
    })

pcFalseH :: VisitWithTag -> Hyp
pcFalseH visit = HypPcImp (PcImpHyp
    { lhs = PcImpHypSidePc visit
    , rhs = PcImpHypSideBool False
    })

pcTrivH :: VisitWithTag -> Hyp
pcTrivH visit = HypPcImp (PcImpHyp
    { lhs = PcImpHypSidePc visit
    , rhs = PcImpHypSidePc visit
    })

pcImpH :: PcImpHypSide -> PcImpHypSide -> Hyp
pcImpH lhs rhs = HypPcImp (PcImpHyp { lhs, rhs })

eqSideH :: Expr -> VisitWithTag -> EqHypSide
eqSideH = EqHypSide

eqInductH :: Integer -> Integer -> EqHypInduct
eqInductH = EqHypInduct
