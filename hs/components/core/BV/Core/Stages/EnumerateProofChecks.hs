{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Use :" #-}

module BV.Core.Stages.EnumerateProofChecks
    ( enumerateProofChecks
    ) where

import BV.Core.ExprConstruction
import BV.Core.Graph
import BV.Core.ProofCheckConstruction
import BV.Core.Types

import BV.Core.Utils (optionals)
import Control.Monad.Reader (Reader, runReader)
import Data.Foldable (fold)
import Data.Function (applyWhen, on, (&))
import Data.Functor ((<&>))
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid (Endo (Endo, appEndo))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import Debug.Trace
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Optics
import Text.ParserCombinators.ReadP (option)
import Text.Printf (printf)

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
             in M.fromList $ flip foldMap heads $ \(head, scc) ->
                    [(head, LoopHead scc)] <> flip mapMaybe (S.toList scc) (\member ->
                        if member == head then Nothing else Just (member, LoopMember head))
        }

data Context
  = Context
      { pairing :: Pairing
      , problem :: Problem
      , argRenames :: ArgRenames
      , nodeTag :: NodeAddr -> Tag
      , loopData :: Map NodeAddr LoopData
      , nodeGraph :: NodeGraph
      }
  deriving (Generic)

data LoopData
  = LoopHead (Set NodeAddr)
  | LoopMember NodeAddr
  deriving (Eq, Generic, Ord, Show)

loopIdM :: NodeAddr -> Reader Context (Maybe NodeAddr)
loopIdM addr = do
    loopData <- gview $ #loopData % at addr
    return (loopData <&> \case
        LoopHead _ -> addr
        LoopMember addr' -> addr')

loopHeadsM :: Reader Context [NodeAddr]
loopHeadsM = do
    loopData <- gview #loopData
    return (mapMaybe (\(k, v) -> case v of
        LoopHead _ -> Just k
        LoopMember _ -> Nothing) (M.toList loopData))

--

proofChecksRecM :: [Restr] -> [Hyp] -> ProofNodeWith () -> Reader Context (ProofNodeWith NodeProofChecks)
proofChecksRecM restrs hyps (ProofNodeWith _ node) = case node of
    ProofNodeLeaf -> do
        checks <- leafChecksM restrs hyps
        return $ ProofNodeWith checks ProofNodeLeaf
    ProofNodeRestr restrNode -> do
        checks <- restrChecksM restrs hyps restrNode
        let restrs' = getProofRestr restrNode.point restrNode.range : restrs
        let hyps' = hyps ++
                [ pcTrivH
                    (tagV restrNode.tag
                        (Visit (Addr restrNode.point)
                            (Restr
                                restrNode.point
                                (fromRestrKindVC restrNode.range.kind (restrNode.range.y - 1))
                            : restrs)))
                ]
        node' <- traverseRestrProofNodeChild
            (proofChecksRecM restrs' hyps')
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
        let noLoopHyps = splitNoLoopHyps splitNode restrs
        let loopHyps = splitLoopHyps splitNode restrs True
        node' <- traverseSplitProofNodeChildren
            (proofChecksRecM restrs (hyps ++ noLoopHyps))
            (proofChecksRecM restrs (hyps ++ loopHyps))
            splitNode
        return $ ProofNodeWith checks (ProofNodeSplit node')
    ProofNodeSingleRevInduct singleRevInductNode -> do
        checks <- singleRevInductChecksM restrs hyps singleRevInductNode
        hyp' <- singleRevInductResultingHypM restrs singleRevInductNode
        node' <- traverseSingleRevInductProofNodeChild
            (proofChecksRecM restrs (hyps ++ [hyp']))
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
restrChecksM restrs hyps restrNode = do
    let restr = getProofRestr restrNode.point restrNode.range
    restrOthers <- restrOthersM (restr : restrs) 2
    let nCErrHyp = nonRErrPcH restrOthers
    let hyps' = nCErrHyp : hyps
    nodeTag <- gview #nodeTag
    let visit vc = tagV (nodeTag restrNode.point)
            (Visit (Addr restrNode.point) ((Restr restrNode.point vc) : restrs))
    let minVC = case restrNode.range.kind of
            RestrProofNodeRangeKindOffset -> Just $ offsetVC (max 0 (restrNode.range.x - 1))
            _ | restrNode.range.x > 1 -> Just $ numberVC (restrNode.range.x - 1)
            _ -> Nothing
    let initCheck = case minVC of
            Just minVC' -> [ProofCheck
                (printf "Check of restr min %d %s for %d" restrNode.range.x (prettyRestrProofNodeRangeKind restrNode.range.kind) restrNode.point.unwrap)
                hyps'
                (pcTrueH (visit minVC'))]
            Nothing -> []
    let topVC = fromRestrKindVC restrNode.range.kind (restrNode.range.y - 1)
    let topCheck = ProofCheck
                (printf "Check of restr max %d %s for %d" restrNode.range.y (prettyRestrProofNodeRangeKind restrNode.range.kind) restrNode.point.unwrap)
                hyps'
                (pcFalseH (visit topVC))
    return $ initCheck ++ [topCheck]

restrOthersM :: [Restr] -> Integer -> Reader Context [Restr]
restrOthersM restrs n = do
    xs <- loopsToSplitM restrs
    let extras = [ Restr sp (upToVC n) | sp <- xs ]
    return $ restrs ++ extras

loopsToSplitM :: [Restr] -> Reader Context [NodeAddr]
loopsToSplitM restrs = do
    loopHeadsWithSplit <- fmap (S.fromList . catMaybes) . for restrs $ \restr -> do
        loopIdM restr.nodeAddr
    loopHeads <- S.fromList <$> loopHeadsM
    let remLoopHeadsInit = loopHeadsWithSplit `S.difference` loopHeads
    g <- gview #nodeGraph
    nodeTag <- gview #nodeTag
    let f :: Restr -> Set NodeAddr -> Set NodeAddr
        f restr = applyWhen (hasZeroVC restr.visitCount) . S.filter $ \lh ->
            isReachableFrom g (Addr restr.nodeAddr) (Addr lh) ||
                nodeTag restr.nodeAddr /= nodeTag lh
    return . S.toList $ appEndo (foldMap (Endo . f) restrs) remLoopHeadsInit

getProofRestr :: NodeAddr -> RestrProofNodeRange -> Restr
getProofRestr point range =
    Restr
        point
        (optionsVC (map (fromRestrKindVC range.kind) [range.x .. range.y - 1]))

splitChecksM :: [Restr] -> [Hyp] -> SplitProofNode () -> Reader Context NodeProofChecks
splitChecksM restrs hyps splitNode = do
    return []

splitNoLoopHyps :: SplitProofNode () -> [Restr] -> [Hyp]
splitNoLoopHyps splitNode restrs =
    [pcFalseH visits.asm]
  where
    visits = splitVisitVisits splitNode restrs (numberVC splitNode.n)

splitVisitVisits :: SplitProofNode () -> [Restr] -> VisitCount -> PairingOf VisitWithTag
splitVisitVisits splitNode restrs visit = withTags splitNode.details <&> \detailsWithTag ->
    splitVisitOneVisit detailsWithTag restrs visit

splitVisitOneVisit :: WithTag SplitProofNodeDetails -> [Restr] -> VisitCount -> VisitWithTag
splitVisitOneVisit detailsWithTag restrs visit = tagV detailsWithTag.tag $
    let restr' = Restr detailsWithTag.value.split visit
     in Visit (Addr detailsWithTag.value.split) (restr' : restrs)

splitHypsAtVisit :: SplitProofNode () -> [Restr] -> VisitCount -> [(Hyp, String)]
splitHypsAtVisit splitNode restrs visit =
    []
  where
    visits = splitVisitVisits splitNode restrs visit
    starts = splitVisitVisits splitNode restrs (numberVC 0)
    mksub v exp = undefined
    inst exp = undefined
    zsub = mksub (machineWordE 0)
    -- lsub = case visit of

splitLoopHyps :: SplitProofNode () -> [Restr] -> Bool -> [Hyp]
splitLoopHyps splitNode restrs exit =
    hyps'
  where
    n = splitNode.n
    visits = splitVisitVisits splitNode restrs (offsetVC (n - 1))
    conts = splitVisitVisits splitNode restrs (offsetVC n)
    lEnter = pcTrueH visits.asm
    lExit = pcFalseH conts.asm
    hyps = [lEnter] ++ optionals exit [lExit]
    hyps' = hyps ++
        [ hyp
        | offs <- [ offsetVC i | i <- [0..n-1] ]
        , (hyp, _) <- splitHypsAtVisit splitNode restrs offs
        ]

singleRevInductChecksM :: [Restr] -> [Hyp] -> SingleRevInductProofNode () -> Reader Context NodeProofChecks
singleRevInductChecksM restrs hyps = undefined

singleRevInductResultingHypM :: [Restr] -> SingleRevInductProofNode () -> Reader Context Hyp
singleRevInductResultingHypM restrs singleRevInductNode = do
    undefined

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
