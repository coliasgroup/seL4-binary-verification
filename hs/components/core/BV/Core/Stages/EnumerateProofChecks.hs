{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}

module BV.Core.Stages.EnumerateProofChecks
    ( enumerateProofChecks
    ) where

import BV.Core.ExprConstruction
import BV.Core.Graph
import BV.Core.Types

import Control.Monad.Reader (Reader, runReader)
import Data.Foldable (fold)
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import Debug.Trace
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Optics

type ArgRenames = PairingEqSideQuadrant -> Ident -> Ident

type NodeProofChecks = [ProofCheck String]

enumerateProofChecks :: ArgRenames -> Pairing -> Problem -> ProofScript () -> ProofScript NodeProofChecks
enumerateProofChecks argRenames pairing problem proofScript =
    ProofScript $ runReader m context
  where
    m = do
        hyps <- initPointHypsM
        proofChecksRecM [] hyps proofScript.root
    nodeGraph = makeNodeGraph (M.toAscList problem.nodes)
    context = Context
        { pairing
        , problem
        , argRenames
        -- , nodeGraph = makeNodeGraph (map (_2 %~ view #node) (M.toAscList problem.nodes))
        , nodeGraph
        , nodeTag =
            let c = S.fromList . mapMaybe (preview #_Addr) $ reachable nodeGraph problem.sides.c.entryPoint
             in \addr -> if addr `S.member` c then C else Asm
        , loopData =
            let heads = loopHeads nodeGraph [problem.sides.c.entryPoint, problem.sides.asm.entryPoint]
                m = M.fromList $ flip foldMap heads $ \(head, scc) ->
                    [(head, LoopHead scc)] <> flip mapMaybe (S.toList scc) (\member ->
                        if member == head then Nothing else Just (member, LoopMember head))
             in (`M.lookup` m)
        }

data Context
  = Context
      { pairing :: Pairing
      , problem :: Problem
      , argRenames :: ArgRenames
      , nodeTag :: NodeAddr -> Tag
      , loopData :: NodeAddr -> Maybe LoopData
      , nodeGraph :: NodeGraph
      }
  deriving (Generic)

data LoopData
  = LoopHead (Set NodeAddr)
  | LoopMember NodeAddr
  deriving (Eq, Generic, Ord, Show)

proofChecksRecM :: [Restr] -> [Hyp] -> ProofNodeWith () -> Reader Context (ProofNodeWith NodeProofChecks)
proofChecksRecM restrs hyps (ProofNodeWith _ node) = case node of
    ProofNodeLeaf -> do
        checks <- leafChecksM restrs hyps
        return $ ProofNodeWith checks ProofNodeLeaf
    ProofNodeRestr restrNode -> do
        checks <- restrChecksM restrs hyps restrNode
        node' <- traverseRestrProofNodeChild
            (proofChecksRecM undefined undefined)
            restrNode
        return $ ProofNodeWith checks (ProofNodeRestr node')
    ProofNodeCaseSplit caseSplitNode -> do
        let visit = tagV caseSplitNode.tag (Visit (Addr caseSplitNode.addr) restrs)
        node' <- traverseCaseSplitProofNodeChildren
            (proofChecksRecM restrs (hyps ++ [pcTrueH visit]))
            (proofChecksRecM restrs (hyps ++ [pcFalseH visit]))
            caseSplitNode
        return $ ProofNodeWith [] (ProofNodeCaseSplit node')
    ProofNodeSplit splitNode -> do
        checks <- splitChecksM restrs hyps splitNode
        node' <- traverseSplitProofNodeChildren
            (proofChecksRecM undefined undefined)
            (proofChecksRecM undefined undefined)
            splitNode
        return $ ProofNodeWith checks (ProofNodeSplit node')
    ProofNodeSingleRevInduct singleRevInductNode -> do
        checks <- singleRevInductChecksM restrs hyps singleRevInductNode
        node' <- traverseSingleRevInductProofNodeChild
            (proofChecksRecM undefined undefined)
            singleRevInductNode
        return $ ProofNodeWith checks (ProofNodeSingleRevInduct node')

leafChecksM :: [Restr] -> [Hyp] -> Reader Context NodeProofChecks
leafChecksM restrs hyps = do
    let nerrPcHyp = nonRErrPcH restrs
    let nlerrPc = pcFalseH . asmV $ Visit Err restrs
    let retEq = eqSideH trueE (asmV (Visit Ret restrs)) `eqH'` eqSideH trueE (cV (Visit Ret restrs))
    let hyps' = nerrPcHyp : hyps
    outEqs <- gview $ #pairing % #outEqs
    let pathCondImp = ProofCheck "Leaf path-cond imp" (hyps' ++ [retEq]) nlerrPc
    instEqs' <- instEqsM restrs outEqs
    let otherChecks = map (ProofCheck "Leaf eq check" (hyps' ++ [nlerrPc, retEq])) instEqs'
    return $ pathCondImp : otherChecks

restrChecksM :: [Restr] -> [Hyp] -> RestrProofNode () -> Reader Context NodeProofChecks
restrChecksM restrs hyps = undefined

splitChecksM :: [Restr] -> [Hyp] -> SplitProofNode () -> Reader Context NodeProofChecks
splitChecksM restrs hyps = undefined

singleRevInductChecksM :: [Restr] -> [Hyp] -> SingleRevInductProofNode () -> Reader Context NodeProofChecks
singleRevInductChecksM restrs hyps = undefined

--

initPointHypsM :: Reader Context [Hyp]
initPointHypsM = do
    inEqs <- gview $ #pairing % #inEqs
    instEqsM [] inEqs

instEqsM :: [Restr] -> [PairingEq] -> Reader Context [Hyp]
instEqsM restrs eqs = do
    entryPoints <- gview $ #problem % #sides % to (fmap (.entryPoint))
    let addrMap quadrant = tagV quadrant.tag $ case quadrant.direction of
            PairingEqDirectionIn -> Visit (pairingSide quadrant.tag entryPoints) []
            PairingEqDirectionOut -> Visit Ret restrs
    renames <- gview $ #argRenames
    let hyps = flip map eqs $ \(PairingEq { lhs, rhs }) ->
            let f eqSide = eqSideH (renameVarsI (renames eqSide.quadrant) eqSide.expr) (addrMap eqSide.quadrant)
             in (eqH' `on` f) lhs rhs
    return hyps

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
