{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.Extras.ProofCheck
    ( SimpleVisitCountView (..)
    , doubleRangeVC
    , enumerateSimpleVC
    , eqH
    , eqH'
    , eqIfAtH
    , eqIfAtH'
    , eqInductByTagH
    , eqInductSingleH
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
    , toMapVC
    , trueIfAt
    , trueIfAt'
    , upToVC
    , withMapVC
    , withMapVCF
    ) where

import BV.Core.Types
import BV.Core.Types.Extras.Expr
import BV.Utils (ensure)

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
fromMapVC = map f . M.toList
  where
    f (nodeAddr, visitCount) = Restr { nodeAddr, visitCount }

--

eqH :: EqHypSide t -> EqHypSide t -> Maybe EqHypInduct -> Hyp t
eqH = eqWithIfAtH False

eqH' :: EqHypSide t -> EqHypSide t -> Hyp t
eqH' lhs rhs = eqH lhs rhs Nothing

eqIfAtH :: EqHypSide t -> EqHypSide t -> Maybe EqHypInduct -> Hyp t
eqIfAtH = eqWithIfAtH True

eqIfAtH' :: EqHypSide t -> EqHypSide t -> Hyp t
eqIfAtH' lhs rhs = eqIfAtH lhs rhs Nothing

eqWithIfAtH :: Bool -> EqHypSide t -> EqHypSide t -> Maybe EqHypInduct -> Hyp t
eqWithIfAtH ifAt lhs rhs induct = HypEq
    { ifAt
    , eq = EqHyp { lhs, rhs, induct }
    }

trueIfAt :: Expr -> WithTag t Visit -> Maybe EqHypInduct -> Hyp t
trueIfAt expr visit = eqIfAtH (eqSideH expr visit) (eqSideH trueE visit)

trueIfAt' :: Expr -> WithTag t Visit -> Hyp t
trueIfAt' expr visit = trueIfAt expr visit Nothing

pcTrueH :: WithTag t Visit -> Hyp t
pcTrueH visit = HypPcImp (PcImpHyp
    { lhs = PcImpHypSideBool True
    , rhs = PcImpHypSidePc visit
    })

pcFalseH :: WithTag t Visit -> Hyp t
pcFalseH visit = HypPcImp (PcImpHyp
    { lhs = PcImpHypSidePc visit
    , rhs = PcImpHypSideBool False
    })

pcTrivH :: WithTag t Visit -> Hyp t
pcTrivH visit = HypPcImp (PcImpHyp
    { lhs = PcImpHypSidePc visit
    , rhs = PcImpHypSidePc visit
    })

pcImpH :: PcImpHypSide t -> PcImpHypSide t -> Hyp t
pcImpH lhs rhs = HypPcImp (PcImpHyp { lhs, rhs })

eqSideH :: Expr -> WithTag t Visit -> EqHypSide t
eqSideH = EqHypSide

-- HACK integer representation matches graph-refine

eqInductByTagH :: RefineTag t => ByTag t NodeAddr -> EqHypInduct
eqInductByTagH addrs = ensure (notElem 0 rawAddrs) $ withByRefineTag EqHypInduct rawAddrs
  where
    rawAddrs = (.unwrap) <$> addrs

eqInductSingleH :: NodeAddr -> EqHypInduct
eqInductSingleH addr = EqHypInduct addr.unwrap 0
