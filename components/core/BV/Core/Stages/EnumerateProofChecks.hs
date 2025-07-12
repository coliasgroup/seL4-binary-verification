module BV.Core.Stages.EnumerateProofChecks
    ( enumerateProofChecks
    ) where

import BV.Core.Graph
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils (optionals)

import Control.Monad.Reader (MonadReader (..), ReaderT, runReader)
import Control.Monad.State (MonadState, StateT (StateT), evalStateT)
import Control.Monad.Writer (WriterT, execWriterT, mapWriterT, tell)
import Data.Foldable (for_, traverse_)
import Data.Function (applyWhen)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid (Endo (Endo, appEndo))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=), (.=))
import Text.Printf (printf)

type NodeProofChecks = [ProofCheck ProofCheckDescription]

enumerateProofChecks :: ArgRenames -> Pairing -> Problem -> ProofScript () -> ProofScript NodeProofChecks
enumerateProofChecks argRenames pairing problem proofScript =
    ProofScript $ runReader (evalStateT m initState) context
  where
    context = initContext argRenames pairing problem
    m = do
        assumeR =<< instantiatePairingEqs PairingEqDirectionIn
        enumerateProofChecksInner proofScript.root

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

initContext :: ArgRenames -> Pairing -> Problem -> Context
initContext argRenames pairing problem = Context
    { pairing
    , problem
    , argRenames
    , nodeGraph
    , nodeTag = nodeTagOf problem nodeGraph
    , loopData = createLoopDataMap problem nodeGraph
    }
  where
    nodeGraph = makeNodeGraph (M.toAscList problem.nodes)

askLoopHead :: MonadReader Context m => NodeAddr -> m (Maybe NodeAddr)
askLoopHead addr = loopHeadOf addr <$> gview #loopData

askLoopHeads :: MonadReader Context m => m [NodeAddr]
askLoopHeads = loopHeadsOf <$> gview #loopData

askNodeGraph :: MonadReader Context m => m NodeGraph
askNodeGraph = gview #nodeGraph

askArgRenames :: MonadReader Context m => m ArgRenames
askArgRenames = gview #argRenames

askEntryPoints :: MonadReader Context m => m (PairingOf NodeId)
askEntryPoints = gview $ #problem % #sides % to (fmap (.entryPoint))

askPairing :: MonadReader Context m => m Pairing
askPairing = gview #pairing

askNodeTag :: MonadReader Context m => NodeAddr -> m Tag
askNodeTag addr = ($ addr) <$> gview #nodeTag

askLookupNodeTag :: MonadReader Context m => m (NodeAddr -> Tag)
askLookupNodeTag = gview #nodeTag

data State
  = State
      { restrs :: [Restr]
      , assumptions :: [Hyp]
      }
  deriving (Generic)

initState :: State
initState = State
    { restrs = []
    , assumptions = []
    }

class (MonadReader Context m, MonadState State m) => MonadChecks m where
    branch :: m a -> m a
    branchRestrs :: m a -> m a
    branchAssumptions :: m a -> m a

instance Monad m => MonadChecks (StateT State (ReaderT Context m)) where

    branch (StateT f) = StateT $ \s -> do
        (a, _) <- f s
        return (a, s)

    branchRestrs m = do
        restrs <- getRestrs
        a <- m
        #restrs .= restrs
        return a

    branchAssumptions m = do
        assumptions <- getAssumptions
        a <- m
        #assumptions .= assumptions
        return a

type CheckWriter = WriterT NodeProofChecks

instance MonadChecks m => MonadChecks (CheckWriter m) where
    branch = mapWriterT branch
    branchRestrs = mapWriterT branchRestrs
    branchAssumptions = mapWriterT branchAssumptions

assumeL :: MonadChecks m => [Hyp] -> m ()
assumeL hyps = #assumptions %= (hyps ++)

assumeR :: MonadChecks m => [Hyp] -> m ()
assumeR hyps = #assumptions %= (++ hyps)

assume1L :: MonadChecks m => Hyp -> m ()
assume1L = assumeL . (:[])

assume1R :: MonadChecks m => Hyp -> m ()
assume1R = assumeR . (:[])

getAssumptions :: MonadChecks m => m [Hyp]
getAssumptions = use #assumptions

restrictL :: MonadChecks m => [Restr] -> m ()
restrictL restrs = #restrs %= (restrs ++)

restrictR :: MonadChecks m => [Restr] -> m ()
restrictR restrs = #restrs %= (++ restrs)

restrict1L :: MonadChecks m => Restr -> m ()
restrict1L = restrictL . (:[])

restrict1R :: MonadChecks m => Restr -> m ()
restrict1R = restrictR . (:[])

getRestrs :: MonadChecks m => m [Restr]
getRestrs = use #restrs

collect :: MonadChecks m => CheckWriter m () -> m NodeProofChecks
collect = execWriterT

conclude :: MonadChecks m => ProofCheckDescription-> Hyp -> CheckWriter m ()
conclude meta hyp = do
    hyps <- getAssumptions
    let check = ProofCheck
            { meta
            , hyps
            , hyp
            }
    tell [check]

concludeWith :: MonadChecks m => ProofCheckDescription-> [Hyp] -> Hyp -> CheckWriter m ()
concludeWith meta hyps hyp = branch $ do
    assumeR hyps
    conclude meta hyp

getVisit :: MonadChecks m => NodeId -> m Visit
getVisit n = Visit n <$> getRestrs

getVisitWithTag :: MonadChecks m => Tag -> NodeId -> m VisitWithTag
getVisitWithTag tag n = tagV tag <$> getVisit n

--

instantiatePairingEqs :: MonadChecks m => PairingEqDirection -> m [Hyp]
instantiatePairingEqs direction = do
    pairing <- askPairing
    let eqs = case direction of
            PairingEqDirectionIn -> pairing.inEqs
            PairingEqDirectionOut -> pairing.outEqs
    let visitFor quadrant = tagV quadrant.tag <$> case quadrant.direction of
            PairingEqDirectionIn -> do
                entryPoint <- pairingSide quadrant.tag <$> askEntryPoints
                return $ Visit entryPoint []
            PairingEqDirectionOut -> do
                getVisit Ret
    renames <- askArgRenames
    let eqSideFor side =
            eqSideH (renameVars (renames side.quadrant) side.expr)
                <$> visitFor side.quadrant
    for eqs $ \PairingEq { lhs, rhs } -> eqH' <$> eqSideFor lhs <*> eqSideFor rhs

--

enumerateProofChecksInner :: MonadChecks m => ProofNodeWith () -> m (ProofNodeWith NodeProofChecks)
enumerateProofChecksInner = go
  where
    go (ProofNodeWith _ node) = case node of
        ProofNodeLeaf -> do
            checks <- collect $ branch emitLeafNodeChecks
            return $ ProofNodeWith checks ProofNodeLeaf
        ProofNodeRestr restrNode -> do
            checks <- collect $ branch $ emitRestrNodeChecks restrNode
            branchRestrs $ do
                applyRestrNodeMax restrNode
                assume1R =<< pcTrivH <$> getVisitWithTag restrNode.tag (Addr restrNode.point)
            applyRestrNodeRange restrNode
            ProofNodeWith checks . ProofNodeRestr <$>
                traverseRestrProofNodeChild go restrNode
        ProofNodeCaseSplit caseSplitNode -> do
            visit <- tagV caseSplitNode.tag <$> getVisit (Addr caseSplitNode.addr)
            ProofNodeWith [] . ProofNodeCaseSplit <$>
                traverseCaseSplitProofNodeChildren
                    (thenGo (assumeR [pcTrueH visit]))
                    (thenGo (assumeR [pcFalseH visit]))
                    caseSplitNode
        ProofNodeSplit splitNode -> do
            checks <- collect $ branch $ splitChecksM splitNode
            ProofNodeWith checks . ProofNodeSplit <$>
                traverseSplitProofNodeChildren
                    (thenGo (splitNoLoopHyps splitNode))
                    (thenGo (splitLoopHyps splitNode True))
                    splitNode
        ProofNodeSingleRevInduct singleRevInductNode -> do
            checks <- collect $ branch $ singleRevInductChecksM singleRevInductNode
            singleRevInductResultingHypM singleRevInductNode
            ProofNodeWith checks . ProofNodeSingleRevInduct <$>
                traverseSingleRevInductProofNodeChild go singleRevInductNode
      where
        thenGo m n = branch $ m >> go n

--

emitLeafNodeChecks :: MonadChecks m => CheckWriter m ()
emitLeafNodeChecks = branch $ do
    assume1L =<< pcFalseH <$> getVisitWithTag C Err
    noAsmErr <- pcFalseH <$> getVisitWithTag Asm Err
    retEq <- eqH'
        <$> (eqSideH trueE <$> getVisitWithTag Asm Ret)
        <*> (eqSideH trueE <$> getVisitWithTag C Ret)
    outEqs <- instantiatePairingEqs PairingEqDirectionOut
    concludeWith "Leaf path-cond imp" [retEq] noAsmErr
    traverse_ (concludeWith "Leaf eq check" [noAsmErr, retEq]) outEqs

--

emitRestrNodeChecks :: MonadChecks m => RestrProofNode () -> CheckWriter m ()
emitRestrNodeChecks restrNode = branch $ do
    branchRestrs $ do
        applyRestrNodeRange restrNode
        assume1L =<< getRestrOtherHyp
    let visit vc = branchRestrs $ do
            tag <- askNodeTag restrNode.point
            restrict1L $ Restr restrNode.point vc
            getVisitWithTag tag $ Addr restrNode.point
    let minVCOpt = case restrNode.range.kind of
            RestrProofNodeRangeKindOffset -> Just $ offsetVC $ max 0 $ restrNode.range.x - 1
            _ | restrNode.range.x > 1 -> Just $ numberVC $ restrNode.range.x - 1
            _ -> Nothing
    for_ minVCOpt $ \minVC -> do
        v <- visit minVC
        conclude
            (printf "Check of restr min %d %s for %d"
                restrNode.range.x
                (prettyRestrProofNodeRangeKind restrNode.range.kind)
                restrNode.point.unwrap)
            (pcTrueH v)
    topVC <- visit $ fromRestrKindVC restrNode.range.kind (restrNode.range.y - 1)
    conclude
        (printf "Check of restr max %d %s for %d"
            restrNode.range.y
            (prettyRestrProofNodeRangeKind restrNode.range.kind)
            restrNode.point.unwrap)
        (pcFalseH topVC)

applyRestrNodeMax :: MonadChecks m => (RestrProofNode a) -> m ()
applyRestrNodeMax restrNode = restrict1L $
    Restr
        restrNode.point
        (fromRestrKindVC restrNode.range.kind (restrNode.range.y - 1))

applyRestrNodeRange :: MonadChecks m => (RestrProofNode a) -> m ()
applyRestrNodeRange restrNode = restrict1L $
    Restr
        restrNode.point
        (optionsVC
            (map
                (fromRestrKindVC restrNode.range.kind)
                [restrNode.range.x .. restrNode.range.y - 1]))

getRestrOtherHyp :: MonadChecks m => m Hyp
getRestrOtherHyp = branch $ do
    loopsToSplit <- getLoopsToSplit
    restrictR [ Restr sp (upToVC 2) | sp <- loopsToSplit ]
    pcFalseH <$> getVisitWithTag C Err

getLoopsToSplit :: MonadChecks m => m [NodeAddr]
getLoopsToSplit = do
    restrs <- getRestrs
    loopHeadsWithSplit <- fmap (S.fromList . catMaybes) . for restrs $ \restr -> askLoopHead restr.nodeAddr
    loopHeads_ <- S.fromList <$> askLoopHeads
    let remLoopHeadsInit = loopHeads_ `S.difference` loopHeadsWithSplit
    g <- askNodeGraph
    lookupNodeTag <- askLookupNodeTag
    let f :: Restr -> Set NodeAddr -> Set NodeAddr
        f restr = applyWhen (not (hasZeroVC restr.visitCount)) . S.filter $ \lh ->
            isReachableFrom g (Addr restr.nodeAddr) (Addr lh) ||
                lookupNodeTag restr.nodeAddr /= lookupNodeTag lh
    return . S.toList $ appEndo (foldMap (Endo . f) (reverse restrs)) remLoopHeadsInit

--

splitChecksM :: MonadChecks m => SplitProofNode () -> CheckWriter m ()
splitChecksM splitNode = do
    branch $ splitInitStepChecksM splitNode
    branch $ splitInductStepChecksM splitNode

splitInitStepChecksM :: MonadChecks m => SplitProofNode () -> CheckWriter m ()
splitInitStepChecksM splitNode = do
    errHyp <- splitRErrPcHypM splitNode
    assume1L errHyp
    for_ [0..splitNode.n - 1] $ \i -> branch $ do
        visits <- splitVisitVisits splitNode (numberVC i)
        visHyps <- splitHypsAtVisit splitNode (numberVC i)
        assume1R $ pcTrueH visits.asm
        assume1R $ pcTrivH visits.c
        for_ visHyps $ \(hyp, desc) ->
            conclude
                (printf "Induct check at visit %d: %s" i desc)
                hyp

splitInductStepChecksM :: MonadChecks m => SplitProofNode () -> CheckWriter m ()
splitInductStepChecksM splitNode = do
    errHyp <- splitRErrPcHypM splitNode
    conts <- splitVisitVisits splitNode (offsetVC splitNode.n)
    assumeL [errHyp, pcTrueH conts.asm, pcTrivH conts.c]
    splitLoopHyps splitNode False
    concs <- splitHypsAtVisit splitNode (offsetVC splitNode.n)
    for_ concs $ \(hyp, desc) ->
        conclude
            (printf "Induct check (%s) at inductive step for %d"
                desc
                splitNode.details.asm.split.unwrap)
            hyp

splitRErrPcHypM :: MonadChecks m => SplitProofNode () -> m Hyp
splitRErrPcHypM splitNode = branchRestrs $ do
    let nc = splitNode.n * splitNode.details.c.step
    let vc = doubleRangeVC (splitNode.details.c.seqStart + nc) (splitNode.loopRMax + 2)
    restrict1L $ Restr splitNode.details.c.split vc
    getRestrOtherHyp

splitNoLoopHyps :: MonadChecks m => SplitProofNode () -> m ()
splitNoLoopHyps splitNode = do
    visits <- splitVisitVisits splitNode (numberVC splitNode.n)
    assumeR [pcFalseH visits.asm]

splitVisitVisits :: MonadChecks m => SplitProofNode () -> VisitCount -> m (PairingOf VisitWithTag)
splitVisitVisits splitNode visit = for (withTags splitNode.details) $ \detailsWithTag ->
    splitVisitOneVisit detailsWithTag visit

splitVisitOneVisit :: MonadChecks m => WithTag SplitProofNodeDetails -> VisitCount -> m VisitWithTag
splitVisitOneVisit detailsWithTag visit = branchRestrs $ do
    let visit' = case fromJust (simpleVC visit) of
            SimpleVisitCountViewOffset n -> offsetVC (n * detailsWithTag.value.step)
            SimpleVisitCountViewNumber n -> numberVC (detailsWithTag.value.seqStart + (n * detailsWithTag.value.step))
    restrict1L $ Restr detailsWithTag.value.split visit'
    getVisitWithTag detailsWithTag.tag (Addr detailsWithTag.value.split)

splitHypsAtVisit :: MonadChecks m => SplitProofNode () -> VisitCount -> m [(Hyp, ProofCheckDescription)]
splitHypsAtVisit splitNode visit = do
    visits <- splitVisitVisits splitNode visit
    starts <- splitVisitVisits splitNode (numberVC 0)
    let mksub v = walkExprs $ \case
            Expr ty (ExprValueVar (Ident "%i")) | isMachineWordT ty -> v
            expr -> expr
        inst expr = instEqAtVisit expr visit
        zsub = mksub (machineWordE 0)
        lsub = mksub $ case fromJust (simpleVC visit) of
            SimpleVisitCountViewNumber n -> machineWordE n
            SimpleVisitCountViewOffset n -> machineWordVarE (Ident "%n") `plusE` machineWordE n
    return $
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

loopEqHypsAtVisit :: MonadChecks m => Tag -> NodeAddr -> [Lambda] -> VisitCount -> Bool -> m [(Hyp, ProofCheckDescription)]
loopEqHypsAtVisit tag split eqs visitNum useIfAt = do
    let details = SplitProofNodeDetails split 0 1 eqs
    visit <- splitVisitOneVisit (WithTag tag details) visitNum
    start <- splitVisitOneVisit (WithTag tag details) (numberVC 0)
    let mksub v = walkExprs $ \case
            Expr ty (ExprValueVar (Ident "%i")) | isMachineWordT ty -> v
            expr -> expr
    let zsub = mksub (machineWordE 0)
    let isub = mksub $ case fromJust (simpleVC visitNum) of
            SimpleVisitCountViewNumber n -> machineWordE n
            SimpleVisitCountViewOffset n -> machineWordVarE (Ident "%n") `plusE` machineWordE n
    return $
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

instEqAtVisit :: Expr -> VisitCount -> Bool
instEqAtVisit expr visit = case expr.value of
    ExprValueOp OpEqSelectiveWrapper [_, xs, ys] ->
        let xs' = word32ListFromExpr xs
            ys' = word32ListFromExpr ys
         in case fromJust (simpleVC visit) of
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

splitLoopHyps :: MonadChecks m => SplitProofNode () -> Bool -> m ()
splitLoopHyps splitNode exit = do
    let n = splitNode.n
    visits <- splitVisitVisits splitNode (offsetVC (n - 1))
    conts <- splitVisitVisits splitNode (offsetVC n)
    let lEnter = pcTrueH visits.asm
    let lExit = pcFalseH conts.asm
    assume1R lEnter
    traverse_ assume1R $ optionals exit [lExit]
    for_ [ offsetVC i | i <- [0..n-1] ] $ \offs -> do
        concs <- splitHypsAtVisit splitNode offs
        for_ concs $ \(hyp, _) -> assume1R hyp

singleRevInductChecksM :: MonadChecks m => SingleRevInductProofNode () -> CheckWriter m ()
singleRevInductChecksM node = do
    tag <- askNodeTag node.point
    sequence_
        [ branch $ singleLoopInductStepChecksM node tag
        , branch $ singleLoopInductBaseChecksM node tag
        , branch $ singleLoopRevInductChecksM node tag
        , branch $ singleLoopRevInductBaseChecksM node tag
        ]

singleLoopInductStepChecksM :: MonadChecks m => SingleRevInductProofNode () -> Tag -> CheckWriter m ()
singleLoopInductStepChecksM node tag = do
    let eqsAssume = []
    let details = SplitProofNodeDetails node.point 0 1 node.eqs
    cont <- splitVisitOneVisit (WithTag tag details) (offsetVC node.n)
    assumeL [pcTrueH cont]
    for_ [0.. node.n - 1] $ \i ->
        loopEqHypsAtVisit tag node.point (eqsAssume ++ node.eqs) (offsetVC i) False
            >>= traverse_ (\(h, _) -> assume1R h)
    loopEqHypsAtVisit tag node.point (eqsAssume ++ node.eqs) (offsetVC node.n) False
        >>= traverse_ (\(hyp, desc) ->
            conclude
                ("Induct check (" ++ desc ++ ") at inductive step for " ++ show node.point.unwrap)
                hyp)

singleLoopInductBaseChecksM :: MonadChecks m => SingleRevInductProofNode () -> Tag -> CheckWriter m ()
singleLoopInductBaseChecksM node tag = do
    let details = SplitProofNodeDetails node.point 0 1 node.eqs
    for_ [0 .. node.n] $ \i -> do
        reach <- splitVisitOneVisit (WithTag tag details) (numberVC i)
        branch $ do
            assumeR [ pcTrueH reach ]
            concs <- loopEqHypsAtVisit tag node.point node.eqs (numberVC i) False
            for_ concs $ \(hyp, desc) ->
                conclude
                    ("Base check (" ++ desc ++ ", " ++ show i ++ ") at induct step for " ++ show node.point.unwrap)
                    hyp

singleLoopRevInductChecksM :: MonadChecks m => SingleRevInductProofNode () -> Tag -> CheckWriter m ()
singleLoopRevInductChecksM node tag = do
    let eqsAssume = node.eqs
    let details = SplitProofNodeDetails node.point 0 1 eqsAssume
    curr <- splitVisitOneVisit (WithTag tag details) (offsetVC 1)
    cont <- splitVisitOneVisit (WithTag tag details) (offsetVC 2)
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
    nonErr <- splitRErrPcHypM splitDetails
    let trueNext = trueIfAt' node.pred_ cont
    assumeR [pcTrueH curr, trueNext, nonErr]
    hyps <- loopEqHypsAtVisit tag node.point eqsAssume (offsetVC 1) True
    for_ hyps $ \(h, _) -> assume1R h
    let goal = trueIfAt' node.pred_ curr
    conclude
        "Pred reverse step."
        goal

mkLoopCounterEqHypM :: MonadChecks m => SingleRevInductProofNode () -> m Hyp
mkLoopCounterEqHypM node = do
    tag <- askNodeTag node.point
    let details = SplitProofNodeDetails node.point 0 1 []
    visit <- splitVisitOneVisit (WithTag tag details) (offsetVC 0)
    return $
        eqH
            (eqSideH (machineWordVarE (Ident "%n")) visit)
            (eqSideH (machineWordE node.nBound) visit)
            (Just (eqInductH node.point.unwrap 0))

singleLoopRevInductBaseChecksM :: MonadChecks m => SingleRevInductProofNode () -> Tag -> CheckWriter m ()
singleLoopRevInductBaseChecksM node tag = do
    let eqsAssume = node.eqs
    let details = SplitProofNodeDetails node.point 0 1 eqsAssume
    cont <- splitVisitOneVisit (WithTag tag details) (offsetVC 1)
    nhyp <- mkLoopCounterEqHypM node
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
    nonErr <- splitRErrPcHypM splitDetails
    assumeR [nhyp, pcTrueH cont, nonErr]
    hyps <- loopEqHypsAtVisit tag node.point eqsAssume (offsetVC 0) False
    for_ hyps $ \(h, _) -> assume1R h
    let goal = trueIfAt' node.pred_ cont
    conclude
        ("Pred true at " ++ show node.nBound ++ " check.")
        goal

singleRevInductResultingHypM :: MonadChecks m => SingleRevInductProofNode () -> m ()
singleRevInductResultingHypM node = branchRestrs $ do
    restrict1R $ Restr node.point (numberVC 0)
    tag <- askNodeTag node.point
    vis <- getVisitWithTag tag (Addr node.point)
    assume1R $ trueIfAt' node.pred_ vis
