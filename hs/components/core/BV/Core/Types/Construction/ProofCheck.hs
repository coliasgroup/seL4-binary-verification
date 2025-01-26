{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.Construction.ProofCheck
    ( SimpleVisitCountView (..)
    , asmV
    , cV
    , doubleRangeVC
    , eqH
    , eqH'
    , eqIfAtH
    , eqIfAtH'
    , eqInductH
    , eqSideH
    , eqWithIfAtH
    , fromRestrKindVC
    , hasZeroVC
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

import Control.DeepSeq (NFData)
import Data.Foldable (fold)
import Data.Function ((&))
import GHC.Generics (Generic)

import BV.Core.Types
import BV.Core.Types.Construction.Expr

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

data SimpleVisitCountView
  = SimpleVisitCountViewNumber Integer
  | SimpleVisitCountViewOffset Integer
  deriving (Eq, Generic, NFData, Ord, Show)

simpleVisitCountView :: VisitCount -> Maybe SimpleVisitCountView
simpleVisitCountView = \case
    VisitCount { numbers = [n], offsets = [] } -> Just (SimpleVisitCountViewNumber n)
    VisitCount { numbers = [], offsets = [n] } -> Just (SimpleVisitCountViewOffset n)
    _ -> Nothing

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
