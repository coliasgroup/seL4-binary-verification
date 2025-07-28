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
    , fromMapVC
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
    , simpleVC
    , tagV
    , toMapVC
    , trueIfAt
    , trueIfAt'
    , upToVC
    , withMapVC
    , withMapVCF
    ) where

import BV.Core.Types
import BV.Core.Types.Extras.Expr
import BV.Core.Utils

import Control.DeepSeq (NFData)
import Control.Monad.Identity (Identity (Identity, runIdentity))
import qualified Data.Map as M
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

simpleVC :: VisitCount -> Maybe SimpleVisitCountView
simpleVC = \case
    VisitCount { numbers = [n], offsets = [] } -> Just (SimpleVisitCountViewNumber n)
    VisitCount { numbers = [], offsets = [n] } -> Just (SimpleVisitCountViewOffset n)
    _ -> Nothing

enumerateSimpleVC :: VisitCount -> [SimpleVisitCountView]
enumerateSimpleVC vc = map SimpleVisitCountViewNumber vc.numbers ++ map SimpleVisitCountViewOffset vc.offsets

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

withMapVC :: (M.Map NodeAddr VisitCount -> M.Map NodeAddr VisitCount) -> [Restr] -> [Restr]
withMapVC f = runIdentity . withMapVCF (Identity . f)

withMapVCF :: Functor f => (M.Map NodeAddr VisitCount -> f (M.Map NodeAddr VisitCount)) -> [Restr] -> f [Restr]
withMapVCF f = fmap fromMapVC . f . toMapVC

toMapVC :: [Restr] -> M.Map NodeAddr VisitCount
toMapVC restrs = ensure check m
  where
    m = M.fromList [ (restr.nodeAddr, restr.visitCount) | restr <- restrs ]
    check = M.size m == length restrs

fromMapVC :: M.Map NodeAddr VisitCount -> [Restr]
fromMapVC = map f . M.toAscList
  where
    f (nodeAddr, visitCount) = Restr { nodeAddr, visitCount }

--

tagV :: Tag' -> Visit -> VisitWithTag
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
