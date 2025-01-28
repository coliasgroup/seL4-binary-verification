{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Use :" #-}

module BV.Core.Stages.EnumerateProofChecks
    ( enumerateProofChecks
    ) where

import BV.Core.Graph
import BV.Core.Types
import BV.Core.Types.Extras

import BV.Core.Utils (optionals)
import Control.Monad.Reader (Reader, runReader)
import Data.Foldable (fold)
import Data.Function (applyWhen, on, (&))
import Data.Functor ((<&>))
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, mapMaybe)
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
            let c = S.fromList . mapMaybe (preview #_Addr) $ reachableFrom nodeGraph problem.sides.c.entryPoint
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
    let remLoopHeadsInit = loopHeads `S.difference` loopHeadsWithSplit
    g <- gview #nodeGraph
    nodeTag <- gview #nodeTag
    let f :: Restr -> Set NodeAddr -> Set NodeAddr
        f restr = applyWhen (not (hasZeroVC restr.visitCount)) . S.filter $ \lh ->
            isReachableFrom g (Addr restr.nodeAddr) (Addr lh) ||
                nodeTag restr.nodeAddr /= nodeTag lh
    return . S.toList $ appEndo (foldMap (Endo . f) (reverse restrs)) remLoopHeadsInit

getProofRestr :: NodeAddr -> RestrProofNodeRange -> Restr
getProofRestr point range =
    Restr
        point
        (optionsVC (map (fromRestrKindVC range.kind) [range.x .. range.y - 1]))

splitChecksM :: [Restr] -> [Hyp] -> SplitProofNode () -> Reader Context NodeProofChecks
splitChecksM restrs hyps splitNode = do
    (<>) <$> splitInitStepChecksM restrs hyps splitNode <*> splitInductStepChecksM restrs hyps splitNode

splitInitStepChecksM :: [Restr] -> [Hyp] -> SplitProofNode () -> Reader Context NodeProofChecks
splitInitStepChecksM restrs hyps splitNode = do
    errHyp <- splitRErrPcHypM splitNode restrs
    let hyps' = errHyp : hyps
    return $ concat
        [ let visits = splitVisitVisits splitNode restrs (numberVC i)
              lpcHyp = pcTrueH visits.asm
              rpcTrivHyp = pcTrivH visits.c
              visHyps = splitHypsAtVisit splitNode restrs (numberVC i)
           in
              [ ProofCheck
                    ("Induct check at visit " ++ show i ++ ": " ++ desc)
                    (hyps' ++ [lpcHyp, rpcTrivHyp])
                    hyp
              | (hyp, desc) <- visHyps
              ]
        | i <- [0..splitNode.n - 1]
        ]

splitInductStepChecksM :: [Restr] -> [Hyp] -> SplitProofNode () -> Reader Context NodeProofChecks
splitInductStepChecksM restrs hyps splitNode = do
    errHyp <- splitRErrPcHypM splitNode restrs
    let conts = splitVisitVisits splitNode restrs (offsetVC splitNode.n)
    let hyps' = [errHyp, pcTrueH conts.asm, pcTrivH conts.c] ++ hyps ++ splitLoopHyps splitNode restrs False
    return
        [ let
           in
              ProofCheck
                  ("Induct check (" ++ desc ++ ") at inductive step for " ++ show splitNode.details.asm.split.unwrap)
                  hyps'
                  hyp
        | (hyp, desc) <- splitHypsAtVisit splitNode restrs (offsetVC splitNode.n)
        ]

splitRErrPcHypM :: SplitProofNode () -> [Restr] -> Reader Context Hyp
splitRErrPcHypM splitNode restrs = do
    let nc = splitNode.n * splitNode.details.c.step
    let vc = doubleRangeVC (splitNode.details.c.seqStart + nc) (splitNode.loopRMax + 2)
    restrs' <- restrOthersM (Restr splitNode.details.c.split vc : restrs) 2
    return $ nonRErrPcH restrs'

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
    let visit' = case fromJust (simpleVisitCountView visit) of
            SimpleVisitCountViewOffset n -> offsetVC (n * detailsWithTag.value.step)
            SimpleVisitCountViewNumber n -> numberVC (detailsWithTag.value.seqStart + (n * detailsWithTag.value.step))
        restr' = Restr detailsWithTag.value.split visit'
     in Visit (Addr detailsWithTag.value.split) (restr' : restrs)

splitHypsAtVisit :: SplitProofNode () -> [Restr] -> VisitCount -> [(Hyp, String)]
splitHypsAtVisit splitNode restrs visit =
    [ (pcImpH (PcImpHypSidePc visits.asm) (PcImpHypSidePc visits.c), "pc imp")
    , (pcImpH (PcImpHypSidePc visits.asm) (PcImpHypSidePc starts.asm), prettyTag Asm ++ " pc imp")
    , (pcImpH (PcImpHypSidePc visits.c) (PcImpHypSidePc starts.c), prettyTag C ++ " pc imp")
    ] ++
    [ ( eqH
            (eqSideH (zsub exprL) starts.asm)
            (eqSideH (lsub exprL) visits.asm)
            (Just (eqInductH splitNode.details.asm.split.unwrap splitNode.details.c.split.unwrap))
      , prettyTag Asm ++ " const"
      )
    | Lambda { expr = exprL } <- splitNode.details.asm.eqs
    , inst exprL
    ] ++
    [ ( eqH
            (eqSideH (zsub exprR) starts.c)
            (eqSideH (lsub exprR) visits.c)
            (Just (eqInductH splitNode.details.asm.split.unwrap splitNode.details.c.split.unwrap))
      , prettyTag C ++ " const"
      )
    | Lambda { expr = exprR } <- splitNode.details.c.eqs
    , inst exprR
    ] ++
    [ ( eqH
            (eqSideH (lsub exprL) visits.asm)
            (eqSideH (lsub exprR) visits.c)
            (Just (eqInductH splitNode.details.asm.split.unwrap splitNode.details.c.split.unwrap))
      , "eq"
      )
    | (Lambda { expr = exprL }, Lambda { expr = exprR }) <- splitNode.eqs
    , inst exprL && inst exprR
    ]
  where
    visits = splitVisitVisits splitNode restrs visit
    starts = splitVisitVisits splitNode restrs (numberVC 0)
    mksub v = walkExprsI $ \case
        Expr ty (ExprValueVar (Ident "%i")) | isMachineWordT ty -> v
        expr -> expr
    inst exp = instEqAtVisit exp visit
    zsub = mksub (machineWordE 0)
    lsub = mksub $ case fromJust (simpleVisitCountView visit) of
        SimpleVisitCountViewNumber n -> machineWordE n
        SimpleVisitCountViewOffset n -> machineWordVarE (Ident "%n") `plusE` machineWordE n

loopEqHypsAtVisit :: Tag -> NodeAddr -> [Lambda] -> [Restr] -> VisitCount -> Bool -> [(Hyp, String)]
loopEqHypsAtVisit tag split eqs restrs visitNum useIfAt =
    [ (pcImpH (PcImpHypSidePc visit) (PcImpHypSidePc start), prettyTag tag ++ " pc imp")
    ] ++
    [ ( eqWithIfAtH useIfAt
            (eqSideH (zsub expr) start)
            (eqSideH (isub expr) visit)
            (Just (eqInductH split.unwrap 0))
      , prettyTag tag ++ " const"
      )
    | Lambda { expr } <- eqs
    , instEqAtVisit expr visitNum
    ]
  where
    details = SplitProofNodeDetails split 0 1 eqs
    visit = splitVisitOneVisit (WithTag tag details) restrs visitNum
    start = splitVisitOneVisit (WithTag tag details) restrs (numberVC 0)
    mksub v = walkExprsI $ \case
        Expr ty (ExprValueVar (Ident "%i")) | isMachineWordT ty -> v
        expr -> expr
    zsub = mksub (machineWordE 0)
    isub = mksub $ case fromJust (simpleVisitCountView visitNum) of
        SimpleVisitCountViewNumber n -> machineWordE n
        SimpleVisitCountViewOffset n -> machineWordVarE (Ident "%n") `plusE` machineWordE n

instEqAtVisit :: Expr -> VisitCount -> Bool
instEqAtVisit expr visit = case expr.value of
    ExprValueOp OpEqSelectiveWrapper [_, xs, ys] ->
        let xs' = word32ListFromExpr xs
            ys' = word32ListFromExpr ys
         in case fromJust (simpleVisitCountView visit) of
                SimpleVisitCountViewNumber n -> n `elem` xs'
                SimpleVisitCountViewOffset n -> n `elem` ys'
    _ -> True

word32ListFromExpr :: Expr -> [Integer]
word32ListFromExpr = go
  where
    go expr = case expr.value of
        ExprValueNum n -> [n]
        ExprValueOp OpPlus [Expr _ (ExprValueNum n), expr'] -> n : go expr'
        _ -> []

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
singleRevInductChecksM restrs hyps node = do
    nodeTag <- gview #nodeTag
    let tag = nodeTag node.point
    concat <$> sequence
        [ singleLoopInductStepChecksM restrs hyps node tag
        , singleLoopInductBaseChecksM restrs hyps node tag
        , singleLoopRevInductChecksM restrs hyps node tag
        , singleLoopRevInductBaseChecksM restrs hyps node tag
        ]

singleLoopInductStepChecksM :: [Restr] -> [Hyp] -> SingleRevInductProofNode () -> Tag -> Reader Context NodeProofChecks
singleLoopInductStepChecksM restrs hyps node tag = do
    let eqsAssume = []
    let details = SplitProofNodeDetails node.point 0 1 node.eqs
    let cont = splitVisitOneVisit (WithTag tag details) restrs (offsetVC node.n)
    let hyps' = [pcTrueH cont] ++ hyps ++
            [ h
            | i <- [0.. node.n - 1]
            , (h, _) <- loopEqHypsAtVisit tag node.point (eqsAssume ++ node.eqs) restrs (offsetVC i) False
            ]
    return
        [ ProofCheck
            ("Induct check (" ++ desc ++ ") at inductive step for " ++ show node.point.unwrap)
            hyps'
            hyp
        | (hyp, desc) <- loopEqHypsAtVisit tag node.point (eqsAssume ++ node.eqs) restrs (offsetVC node.n) False
        ]

singleLoopInductBaseChecksM :: [Restr] -> [Hyp] -> SingleRevInductProofNode () -> Tag -> Reader Context NodeProofChecks
singleLoopInductBaseChecksM restrs hyps node tag = do
    let details = SplitProofNodeDetails node.point 0 1 node.eqs
    return . concat $
        [ let reach = splitVisitOneVisit (WithTag tag details) restrs (numberVC i)
              nhyps = [ pcTrueH reach ]
           in
            [ ProofCheck
                ("Base check (" ++ desc ++ ", " ++ show i ++ ") at induct step for " ++ show node.point.unwrap)
                (hyps ++ nhyps)
                hyp
            | (hyp, desc) <- loopEqHypsAtVisit tag node.point node.eqs restrs (numberVC i) False
            ]
        | i <- [0 .. node.n]
        ]

singleLoopRevInductChecksM :: [Restr] -> [Hyp] -> SingleRevInductProofNode () -> Tag -> Reader Context NodeProofChecks
singleLoopRevInductChecksM restrs hyps node tag = do
    let eqsAssume = node.eqs
    let details = SplitProofNodeDetails node.point 0 1 eqsAssume
    let curr = splitVisitOneVisit (WithTag tag details) restrs (offsetVC 1)
    let cont = splitVisitOneVisit (WithTag tag details) restrs (offsetVC 2)
    let splitDetails = SplitProofNode
            { n = 1
            , loopRMax = 1
            , details = PairingOf
                { asm = undefined
                , c = details
                }
            , eqs = undefined
            , p1 = undefined
            , p2 = undefined
            }
    nonErr <- splitRErrPcHypM splitDetails restrs
    let trueNext = trueIfAt' node.pred_ cont
    let hyps' = hyps ++ [pcTrueH curr, trueNext, nonErr] ++
            [ h
            | (h, _) <- loopEqHypsAtVisit tag node.point eqsAssume restrs (offsetVC 1) True
            ]
    let goal = trueIfAt' node.pred_ curr
    return
        [ProofCheck
            "Pred reverse step."
            hyps'
            goal]

mkLoopCounterEqHypM :: SingleRevInductProofNode () -> [Restr] -> Reader Context Hyp
mkLoopCounterEqHypM node restrs = do
    nodeTag <- gview #nodeTag
    let tag = nodeTag node.point
    let details = SplitProofNodeDetails node.point 0 1 []
    let visit = splitVisitOneVisit (WithTag tag details) restrs (offsetVC 0)
    return $
        eqH
            (eqSideH (machineWordVarE (Ident "%n")) visit)
            (eqSideH (machineWordE node.nBound) visit)
            (Just (eqInductH node.point.unwrap 0))

singleLoopRevInductBaseChecksM :: [Restr] -> [Hyp] -> SingleRevInductProofNode () -> Tag -> Reader Context NodeProofChecks
singleLoopRevInductBaseChecksM restrs hyps node tag = do
    let eqsAssume = node.eqs
    let details = SplitProofNodeDetails node.point 0 1 eqsAssume
    let cont = splitVisitOneVisit (WithTag tag details) restrs (offsetVC 1)
    nhyp <- mkLoopCounterEqHypM node restrs
    let splitDetails = SplitProofNode
            { n = 1
            , loopRMax = 1
            , details = PairingOf
                { asm = undefined
                , c = details
                }
            , eqs = undefined
            , p1 = undefined
            , p2 = undefined
            }
    nonErr <- splitRErrPcHypM splitDetails restrs
    let hyps' = hyps ++ [nhyp, pcTrueH cont, nonErr] ++
            [ h
            | (h, _) <- loopEqHypsAtVisit tag node.point eqsAssume restrs (offsetVC 0) False
            ]
    let goal = trueIfAt' node.pred_ cont
    return
        [ProofCheck
            ("Pred true at " ++ show node.nBound ++ " check.")
            hyps'
            goal]

singleRevInductResultingHypM :: [Restr] -> SingleRevInductProofNode () -> Reader Context Hyp
singleRevInductResultingHypM restrs node = do
    nodeTag <- gview #nodeTag
    let tag = nodeTag node.point
    let vis = tagV tag $ Visit (Addr node.point) $ restrs ++ [Restr node.point (numberVC 0)]
    return $ trueIfAt' node.pred_ vis

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
