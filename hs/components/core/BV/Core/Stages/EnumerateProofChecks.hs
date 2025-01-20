module BV.Core.Stages.EnumerateProofChecks
    ( enumerateProofChecks
    ) where

import BV.Core.ExprConstruction
import BV.Core.Types

import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor ((<&>))

type ArgRenames = PairingEqSideQuadrant -> Ident -> Ident

enumerateProofChecks :: ArgRenames -> Pairing -> Problem -> ProofScript () -> ProofScript [ProofCheck a]
enumerateProofChecks argRenames pairing problem proofNode =
    undefined
  where

--

-- instEqs :: ArgRenames -> [Restr] -> [PairingEq] ->

--

nonRErrPcH :: [Restr] -> Hyp
nonRErrPcH restrs = pcFalseH . cV $ Visit Err restrs

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

eqSideH :: Expr -> VisitWithTag -> EqHypSide
eqSideH = EqHypSide

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

fromRestrKindVC :: RestrProofNodeRangeKind -> Integer -> VisitCount
fromRestrKindVC kind n = n & case kind of
    RestrProofNodeRangeKindNumber -> numberVC
    RestrProofNodeRangeKindOffset -> offsetVC

upToVC :: Integer -> VisitCount
upToVC n = optionsVC (map numberVC [0 .. n -1])

hasZeroVC :: VisitCount -> Bool
hasZeroVC vc = 0 `elem` vc.numbers

tagV :: Tag -> Visit -> VisitWithTag
tagV tag visit = VisitWithTag visit tag

cV :: Visit -> VisitWithTag
cV = tagV C

asmV :: Visit -> VisitWithTag
asmV = tagV Asm
