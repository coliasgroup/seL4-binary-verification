{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Functor law" #-}

module BV.Core.Stages.EnumerateProofChecks
    ( enumerateProofChecks
    , pruneProofCheck
    ) where

import BV.Core.Graph
import BV.Core.Logic (instEqAtVisit)
import BV.Core.Types
import BV.Core.Types.Extras

import Control.Monad (when)
import Control.Monad.Reader (MonadReader (..), ReaderT, runReader)
import Control.Monad.State (MonadState, StateT (StateT), evalStateT)
import Control.Monad.Writer (WriterT, execWriterT, mapWriterT, tell)
import Data.Foldable (for_, traverse_)
import Data.Function (applyWhen, on)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=), (.=))
import Text.Printf (printf)

type NodeProofChecks t = [ProofCheck t ProofCheckDescription]

enumerateProofChecks :: RefineTag t => ArgRenames t -> Pairing t -> Problem t -> ProofScript t () -> ProofScript t (NodeProofChecks t)
enumerateProofChecks argRenames pairing problem proofScript =
    ProofScript $ runReader (evalStateT m initState) context
  where
    context = initContext argRenames pairing problem
    m = enumerateProofChecksInner
        (assumeR =<< instantiatePairingEqs PairingEqDirectionIn)
        proofScript.root

pruneProofCheck :: RefineTag t => Problem t -> ProofCheck t a -> ProofCheck t a
pruneProofCheck problem = over checkVisits pruneVisitWithTag
  where
    nodeGraph = makeNodeGraph (M.toAscList problem.nodes)
    nodeTag = (M.!) (nodeTagMap problem nodeGraph)
    pruneVisitWithTag (WithTag tag (Visit nodeId restrs)) = WithTag tag (Visit nodeId (filter (testRestr tag) restrs))
    testRestr tag (Restr nodeAddr _) = nodeTag nodeAddr == tag

data Context t
  = Context
      { pairing :: Pairing t
      , problem :: Problem t
      , argRenames :: ArgRenames t
      , loopData :: ByTag t LoopDataMap
      , nodeGraph :: NodeGraph
      }
  deriving (Generic)

initContext :: RefineTag t => ArgRenames t -> Pairing t -> Problem t -> Context t
initContext argRenames pairing problem = Context
    { pairing
    , problem
    , argRenames
    , nodeGraph
    , loopData = createLoopDataMap problem nodeGraph
    }
  where
    nodeGraph = makeNodeGraph (M.toAscList problem.nodes)

askLoopHead :: (Tag t, MonadReader (Context t) m) => WithTag t NodeAddr -> m (Maybe (WithTag t NodeAddr))
askLoopHead n = fmap (WithTag n.tag) . loopHeadOf n.value <$> gview (#loopData % atTag n.tag)

askLoopHeads :: (Tag t, MonadReader (Context t) m) => m [WithTag t NodeAddr]
askLoopHeads = do
    loopData <- gview #loopData
    return $ concatMap (tagAll . fmap loopHeadsOf) $ (withTags loopData)
  where
    tagAll (WithTag tag xs) = map (WithTag tag) xs

askNodeGraph :: MonadReader (Context t) m => m NodeGraph
askNodeGraph = gview #nodeGraph

askArgRenames :: MonadReader (Context t) m => m (ArgRenames t)
askArgRenames = gview #argRenames

askEntryPoints :: MonadReader (Context t) m => m (ByTag t NodeId)
askEntryPoints = gview $ #problem % #sides % to (fmap (.entryPoint))

askPairing :: MonadReader (Context t) m => m (Pairing t)
askPairing = gview #pairing

data State t
  = State
      { restrs :: [WithTag t Restr]
      , assumptions :: [Hyp t]
      }
  deriving (Generic)

initState :: State t
initState = State
    { restrs = []
    , assumptions = []
    }

class (RefineTag t, MonadReader (Context t) m, MonadState (State t) m) => MonadChecks t m where
    branch :: m a -> m a
    branchRestrs :: m a -> m a
    branchAssumptions :: m a -> m a

instance (RefineTag t, Monad m) => MonadChecks t (StateT (State t) (ReaderT (Context t) m)) where

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

type CheckWriter t = WriterT (NodeProofChecks t)

instance MonadChecks t m => MonadChecks t (CheckWriter t m) where
    branch = mapWriterT branch
    branchRestrs = mapWriterT branchRestrs
    branchAssumptions = mapWriterT branchAssumptions

assumeL :: MonadChecks t m => [Hyp t] -> m ()
assumeL hyps = #assumptions %= (hyps ++)

assumeR :: MonadChecks t m => [Hyp t] -> m ()
assumeR hyps = #assumptions %= (++ hyps)

assume1L :: MonadChecks t m => Hyp t -> m ()
assume1L = assumeL . (:[])

assume1R :: MonadChecks t m => Hyp t -> m ()
assume1R = assumeR . (:[])

getAssumptions :: MonadChecks t m => m [Hyp t]
getAssumptions = use #assumptions

restrictL :: MonadChecks t m => t -> [Restr] -> m ()
restrictL tag restrs = #restrs %= ((map (WithTag tag) restrs) ++)

restrictR :: MonadChecks t m => t -> [Restr] -> m ()
restrictR tag restrs = #restrs %= (++ (map (WithTag tag) restrs))

restrict1L :: MonadChecks t m => t -> Restr -> m ()
restrict1L tag = restrictL tag . (:[])

restrict1R :: MonadChecks t m => t -> Restr -> m ()
restrict1R tag = restrictR tag . (:[])

getRestrs :: MonadChecks t m => m [WithTag t Restr]
getRestrs = use #restrs

getRestrsForTag :: MonadChecks t m => t -> m [Restr]
getRestrsForTag _t = mapMaybe f <$> use #restrs
  where
    -- TODO HACK to match grap-refine
    -- f x = if x.tag == t then Just x.value else Nothing
    f x = Just x.value

collect :: MonadChecks t m => CheckWriter t m () -> m (NodeProofChecks t)
collect = execWriterT

conclude :: MonadChecks t m => ProofCheckDescription -> Hyp t -> CheckWriter t m ()
conclude meta hyp = do
    hyps <- getAssumptions
    let check = ProofCheck
            { meta
            , hyps
            , hyp
            }
    tell [check]

concludeWith :: MonadChecks t m => ProofCheckDescription -> [Hyp t] -> Hyp t -> CheckWriter t m ()
concludeWith meta hyps hyp = branch $ do
    assumeR hyps
    conclude meta hyp

getVisitWithTag :: MonadChecks t m => t -> NodeId -> m (WithTag t Visit)
getVisitWithTag tag n = WithTag tag . Visit n <$> getRestrsForTag tag

--

data HypWithDesc t
  = HypWithDesc
      { desc :: String
      , hyp :: Hyp t
      }
  deriving (Generic)

concludeManyWith :: MonadChecks t m => (String -> String) -> [HypWithDesc t] -> CheckWriter t m ()
concludeManyWith f hyps = for_ hyps $ \hyp -> conclude (f hyp.desc) hyp.hyp

assumeHyps :: MonadChecks t m => [HypWithDesc t] -> m ()
assumeHyps = assumeR . map (.hyp)

--

instantiatePairingEqs :: MonadChecks t m => PairingEqDirection -> m [Hyp t]
instantiatePairingEqs direction = branch $ do
    pairing <- askPairing
    let eqs = case direction of
            PairingEqDirectionIn -> pairing.inEqs
            PairingEqDirectionOut -> pairing.outEqs
    let visitFor quadrant = tagV quadrant.tag <$> case quadrant.direction of
            PairingEqDirectionIn -> do
                entryPoint <- viewAtTag quadrant.tag <$> askEntryPoints
                return $ Visit entryPoint []
            PairingEqDirectionOut -> do
                Visit Ret <$> getRestrsForTag quadrant.tag
    renames <- askArgRenames
    let eqSideFor side =
            eqSideH (renameVars (renames side.quadrant) side.expr)
                <$> visitFor side.quadrant
    for eqs $ \PairingEq { lhs, rhs } -> eqH' <$> eqSideFor lhs <*> eqSideFor rhs

--

enumerateProofChecksInner :: MonadChecks t m => m () -> ProofNodeWith t () -> m (ProofNodeWith t (NodeProofChecks t))
enumerateProofChecksInner = go
  where
    go before (ProofNodeWith () node) = branch $ do
        before
        case node of
            ProofNodeLeaf -> do
                checks <- collect $ branch emitLeafNodeChecks
                return $ ProofNodeWith checks ProofNodeLeaf
            ProofNodeRestr restrNode -> do
                checks <- collect $ branch $ emitRestrNodeChecks restrNode
                ProofNodeWith checks . ProofNodeRestr <$>
                    traverseRestrProofNodeChild
                        (go (assumeRestrTriv restrNode >> applyRestrNodeRange restrNode))
                        restrNode
            ProofNodeCaseSplit caseSplitNode -> do
                visit <- getVisitWithTag caseSplitNode.tag (Addr caseSplitNode.addr)
                ProofNodeWith [] . ProofNodeCaseSplit <$>
                    traverseCaseSplitProofNodeChildren
                        (go (assume1R (pcTrueH visit)))
                        (go (assume1R (pcFalseH visit)))
                        caseSplitNode
            ProofNodeSplit splitNode -> do
                checks <- collect $ branch $ emitSplitNodeChecks splitNode
                ProofNodeWith checks . ProofNodeSplit <$>
                    traverseSplitProofNodeChildren
                        (go (assumeSplitNoLoop splitNode))
                        (go (assumeSplitLoop splitNode True))
                        splitNode
            ProofNodeSingleRevInduct singleRevInductNode -> do
                checks <- collect $ branch $ emitSingleRevInductNodeChecks singleRevInductNode
                ProofNodeWith checks . ProofNodeSingleRevInduct <$>
                    traverseSingleRevInductProofNodeChild
                        (go (assumeSingleRevInduct singleRevInductNode))
                        singleRevInductNode

--

emitLeafNodeChecks :: MonadChecks t m => CheckWriter t m ()
emitLeafNodeChecks = branch $ do
    assume1L =<< pcFalseH <$> getVisitWithTag rightTag Err
    noAsmErr <- pcFalseH <$> getVisitWithTag leftTag Err
    retEq <- eqH'
        <$> (eqSideH trueE <$> getVisitWithTag leftTag Ret)
        <*> (eqSideH trueE <$> getVisitWithTag rightTag Ret)
    outEqs <- instantiatePairingEqs PairingEqDirectionOut
    concludeWith "Leaf path-cond imp" [retEq] noAsmErr
    traverse_ (concludeWith "Leaf eq check" [noAsmErr, retEq]) outEqs

--

emitRestrNodeChecks :: MonadChecks t m => RestrProofNode t () -> CheckWriter t m ()
emitRestrNodeChecks restrNode = branch $ do
    branchRestrs $ do
        applyRestrNodeRange restrNode
        applyRestrOthers
        assume1L =<< pcFalseH <$> getVisitWithTag rightTag Err
    let visit vc = branchRestrs $ do
            restrict1L restrNode.tag $ Restr restrNode.point vc
            getVisitWithTag restrNode.tag $ Addr restrNode.point
    let minPred = restrNode.range.x - 1
    let minPredVCOpt = case restrNode.range.kind of
            RestrProofNodeRangeKindOffset -> Just $ offsetVC (max 0 minPred)
            _ | minPred > 0 -> Just $ numberVC minPred
            _ -> Nothing
    for_ minPredVCOpt $ \minPredVC -> do
        v <- visit minPredVC
        conclude
            (printf "Check of restr min %d %s for %d"
                restrNode.range.x
                (prettyRestrProofNodeRangeKind restrNode.range.kind)
                restrNode.point)
            (pcTrueH v)
    maxVC <- visit $ maxVCForRestrNode restrNode.range
    conclude
        (printf "Check of restr max %d %s for %d"
            restrNode.range.y
            (prettyRestrProofNodeRangeKind restrNode.range.kind)
            restrNode.point)
        (pcFalseH maxVC)

assumeRestrTriv :: MonadChecks t m => RestrProofNode t a -> m ()
assumeRestrTriv restrNode = branchRestrs $ do
    applyRestrNodeMax restrNode
    assume1R =<< pcTrivH <$> getVisitWithTag restrNode.tag (Addr restrNode.point)

applyRestrNodeMax :: MonadChecks t m => RestrProofNode t a -> m ()
applyRestrNodeMax restrNode = restrict1L restrNode.tag $
    Restr
        restrNode.point
        (maxVCForRestrNode restrNode.range)

maxVCForRestrNode :: RestrProofNodeRange -> VisitCount
maxVCForRestrNode range = fromRestrKindVC range.kind (range.y - 1)

applyRestrNodeRange :: MonadChecks t m => RestrProofNode t a -> m ()
applyRestrNodeRange restrNode = restrict1L restrNode.tag $
    Restr
        restrNode.point
        (foldMap
            (fromRestrKindVC restrNode.range.kind)
            [restrNode.range.x .. restrNode.range.y - 1])

applyRestrOthers :: forall t m. MonadChecks t m => m ()
applyRestrOthers = do
    restrs <- getRestrs
    loopsToSplit <- askLoopsToSplit restrs
    for_ loopsToSplit $ \(WithTag tag addr) -> restrict1R tag (Restr addr (numbersVC [0, 1]))
  where
    askLoopsToSplit :: [WithTag t Restr] -> m [WithTag t NodeAddr]
    askLoopsToSplit restrs = do
        loopHeadsWithSplit <- fmap catMaybes $ for restrs $ \restr -> askLoopHead (fmap (.nodeAddr) restr)
        loopHeads <- askLoopHeads
        let loopHeadsWithoutSplit = (S.difference `on` S.fromList) loopHeads loopHeadsWithSplit
        g <- askNodeGraph
        let f :: WithTag t Restr -> S.Set (WithTag t NodeAddr) -> S.Set (WithTag t NodeAddr)
            f restr = applyWhen (not (hasZeroVC restr.value.visitCount)) $ S.filter $ \loopHeadWithoutSplit ->
                isReachableFrom g (Addr restr.value.nodeAddr) (Addr loopHeadWithoutSplit.value)
                    || restr.tag /= loopHeadWithoutSplit.tag
        return $ S.toList $ foldr f loopHeadsWithoutSplit (reverse restrs)

--

emitSplitNodeChecks :: MonadChecks t m => SplitProofNode t () -> CheckWriter t m ()
emitSplitNodeChecks splitNode = branch $ do
    branch $ emitSplitNodeInitStepChecks splitNode
    branch $ emitSplitNodeInductStepChecks splitNode

emitSplitNodeInitStepChecks :: MonadChecks t m => SplitProofNode t () -> CheckWriter t m ()
emitSplitNodeInitStepChecks splitNode = branch $ do
    assume1L =<< getSplitNodeCErrHyp splitNode
    for_ [0 .. splitNode.n - 1] $ \i -> branch $ do
        visits <- getSplitVisitsAt splitNode (numberVC i)
        assumeR [pcTrueH (getLeft visits), pcTrivH (getRight visits)]
        getSplitHypsAt splitNode (numberVC i)
            >>= concludeManyWith (\desc -> printf "Induct check at visit %d: %s" i desc)

emitSplitNodeInductStepChecks :: MonadChecks t m => SplitProofNode t () -> CheckWriter t m ()
emitSplitNodeInductStepChecks splitNode = branch $ do
    let n = splitNode.n
    conts <- getSplitVisitsAt splitNode (offsetVC n)
    assumeL [pcTrueH (getLeft conts), pcTrivH (getRight conts)]
    assume1L =<< getSplitNodeCErrHyp splitNode
    assumeSplitLoop splitNode False
    getSplitHypsAt splitNode (offsetVC n)
        >>= concludeManyWith (\desc ->
            printf "Induct check (%s) at inductive step for %d"
                desc
                (getLeft splitNode.details).split)

getSplitNodeCErrHyp :: MonadChecks t m => SplitProofNode t () -> m (Hyp t)
getSplitNodeCErrHyp splitNode = branch $ do
    restrict1L rightTag $
        Restr (getRight splitNode.details).split $
            doubleRangeVC
                ((getRight splitNode.details).seqStart + (splitNode.n * (getRight splitNode.details).step))
                (splitNode.loopRMax + 2)
    applyRestrOthers
    pcFalseH <$> getVisitWithTag rightTag Err

assumeSplitNoLoop :: MonadChecks t m => SplitProofNode t () -> m ()
assumeSplitNoLoop splitNode = branchRestrs $ do
    visits <- getSplitVisitsAt splitNode (numberVC splitNode.n)
    assume1R $ pcFalseH (getLeft visits)

getSplitVisitsAt :: MonadChecks t m => SplitProofNode t () -> VisitCount -> m (ByTag t (WithTag t Visit))
getSplitVisitsAt splitNode visit = for (withTags splitNode.details) $ \detailsWithTag ->
    getSplitVisitAt detailsWithTag visit

getSplitVisitAt :: MonadChecks t m => WithTag t SplitProofNodeDetails -> VisitCount -> m (WithTag t Visit)
getSplitVisitAt (WithTag tag details) visit = branch $ do
    restrict1L tag $
        Restr details.split $
            case fromJust (simpleVC visit) of
                SimpleVisitCountViewOffset n ->
                    offsetVC $ n * details.step
                SimpleVisitCountViewNumber n ->
                    numberVC $ details.seqStart + (n * details.step)
    getVisitWithTag tag (Addr details.split)

assumeSplitLoop :: MonadChecks t m => SplitProofNode t () -> Bool -> m ()
assumeSplitLoop splitNode exit = branchRestrs $ do
    let n = splitNode.n
    visits <- getSplitVisitsAt splitNode (offsetVC (n - 1))
    conts <- getSplitVisitsAt splitNode (offsetVC n)
    assume1R $ pcTrueH (getLeft visits)
    when exit $ assume1R $ pcFalseH (getLeft conts)
    for_ [0 .. n - 1] $ \i ->
        assumeHyps =<< getSplitHypsAt splitNode (offsetVC i)

getSplitHypsAt :: forall t m. MonadChecks t m => SplitProofNode t () -> VisitCount -> m [HypWithDesc t]
getSplitHypsAt splitNode visit = branch $ do
    visits <- getSplitVisitsAt splitNode visit
    starts <- getSplitVisitsAt splitNode (numberVC 0)
    let mksub v = walkExprs $ \case
            Expr ty (ExprValueVar (Ident "%i")) | isMachineWordT ty -> v
            expr -> expr
        inst expr = instEqAtVisit expr visit
        zsub = mksub $ machineWordE 0
        lsub = mksub $ case fromJust (simpleVC visit) of
            SimpleVisitCountViewNumber n -> machineWordE n
            SimpleVisitCountViewOffset n -> machineWordVarE (Ident "%n") `plusE` machineWordE n
        imp l r = pcImpH (PcImpHypSidePc l) (PcImpHypSidePc r)
        induct = Just $
            eqInductH
                (getLeft splitNode.details).split.unwrap
                (getRight splitNode.details).split.unwrap
    return $
        [ HypWithDesc "pc imp" $ imp (getLeft visits) (getRight visits)
        , HypWithDesc (prettyTag (leftTag :: t) ++ " pc imp") $ imp (getLeft visits) (getLeft starts)
        , HypWithDesc (prettyTag (rightTag :: t) ++ " pc imp") $ imp (getRight visits) (getRight starts)
        ] ++
        [ HypWithDesc (prettyTag (leftTag :: t) ++ " const") $
            eqH
                (eqSideH (zsub exprL) (getLeft starts))
                (eqSideH (lsub exprL) (getLeft visits))
                induct
        | Lambda { expr = exprL } <- (getLeft splitNode.details).eqs
        , inst exprL
        ] ++
        [ HypWithDesc (prettyTag (rightTag :: t) ++ " const") $
            eqH
                (eqSideH (zsub exprR) (getRight starts))
                (eqSideH (lsub exprR) (getRight visits))
                induct
        | Lambda { expr = exprR } <- (getRight splitNode.details).eqs
        , inst exprR
        ] ++
        [ HypWithDesc "eq" $
            eqH
                (eqSideH (lsub exprL) (getLeft visits))
                (eqSideH (lsub exprR) (getRight visits))
                induct
        | (Lambda { expr = exprL }, Lambda { expr = exprR }) <- splitNode.eqs
        , inst exprL && inst exprR
        ]

--

emitSingleRevInductNodeChecks :: MonadChecks t m => SingleRevInductProofNode t () -> CheckWriter t m ()
emitSingleRevInductNodeChecks node = branch $ do
    sequence_
        [ branch $ emitSingleLoopInductStepChecks node
        , branch $ emitSingleLoopInductBaseChecks node
        , branch $ emitSingleLoopRevInductChecks node
        , branch $ emitSingleLoopRevInductBaseChecks node
        ]

emitSingleLoopInductStepChecks :: MonadChecks t m => SingleRevInductProofNode t () -> CheckWriter t m ()
emitSingleLoopInductStepChecks node = branch $ do
    let details = SplitProofNodeDetails node.point 0 1 node.eqs
    assume1L =<< pcTrueH <$> getSplitVisitAt (WithTag node.tag details) (offsetVC node.n)
    for_ [0.. node.n - 1] $ \i ->
        assumeHyps =<< getLoopEqHypsAt node.tag node.point node.eqs (offsetVC i) False
    getLoopEqHypsAt node.tag node.point node.eqs (offsetVC node.n) False
        >>= concludeManyWith (\desc ->
            printf "Induct check (%s) at inductive step for %d"
                desc
                node.point)

emitSingleLoopInductBaseChecks :: MonadChecks t m => SingleRevInductProofNode t () -> CheckWriter t m ()
emitSingleLoopInductBaseChecks node = branch $ do
    let details = SplitProofNodeDetails node.point 0 1 node.eqs
    for_ [0 .. node.n] $ \i -> branch $ do
        assume1R =<< pcTrueH <$> getSplitVisitAt (WithTag node.tag details) (numberVC i)
        getLoopEqHypsAt node.tag node.point node.eqs (numberVC i) False
            >>= concludeManyWith
                (\desc -> printf "Base check (%s, %d) at induct step for %d" desc i node.point)

emitSingleLoopRevInductChecks :: MonadChecks t m => SingleRevInductProofNode t () -> CheckWriter t m ()
emitSingleLoopRevInductChecks node = branch $ do
    let details = SplitProofNodeDetails node.point 0 1 node.eqs
    curr <- getSplitVisitAt (WithTag node.tag details) (offsetVC 1)
    cont <- getSplitVisitAt (WithTag node.tag details) (offsetVC 2)
    assume1R $ pcTrueH curr
    assume1R $ trueIfAt' node.pred_ cont
    assume1R =<< getSingleLoopRevCErrHyp details
    assumeHyps =<< getLoopEqHypsAt node.tag node.point node.eqs (offsetVC 1) True
    conclude
        "Pred reverse step."
        (trueIfAt' node.pred_ curr)

emitSingleLoopRevInductBaseChecks :: MonadChecks t m => SingleRevInductProofNode t () -> CheckWriter t m ()
emitSingleLoopRevInductBaseChecks node = branch $ do
    let details = SplitProofNodeDetails node.point 0 1 node.eqs
    cont <- getSplitVisitAt (WithTag node.tag details) (offsetVC 1)
    assume1R =<< getLoopCounterEqHyp node
    assume1R $ pcTrueH cont
    assume1R =<< getSingleLoopRevCErrHyp details
    assumeHyps =<< getLoopEqHypsAt node.tag node.point node.eqs (offsetVC 0) False
    conclude
        (printf "Pred true at %d check." node.nBound)
        (trueIfAt' node.pred_ cont)

getLoopCounterEqHyp :: MonadChecks t m => SingleRevInductProofNode t () -> m (Hyp t)
getLoopCounterEqHyp node = do
    let details = SplitProofNodeDetails node.point 0 1 []
    visit <- getSplitVisitAt (WithTag node.tag details) (offsetVC 0)
    return $
        eqH
            (eqSideH (machineWordVarE (Ident "%n")) visit)
            (eqSideH (machineWordE node.nBound) visit)
            (Just (eqInductH node.point.unwrap 0))

getSingleLoopRevCErrHyp :: MonadChecks t m => SplitProofNodeDetails -> m (Hyp t)
getSingleLoopRevCErrHyp details = getSplitNodeCErrHyp (SplitProofNode
    { n = 1
    , loopRMax = 1
    , details = byRefineTag undefined details
    , eqs = undefined
    , p1 = undefined
    , p2 = undefined
    })

assumeSingleRevInduct :: MonadChecks t m => SingleRevInductProofNode t () -> m ()
assumeSingleRevInduct node = branchRestrs $ do
    restrict1R node.tag $ Restr node.point (numberVC 0)
    visit <- getVisitWithTag node.tag (Addr node.point)
    assume1R $ trueIfAt' node.pred_ visit

getLoopEqHypsAt :: MonadChecks t m => t -> NodeAddr -> [Lambda] -> VisitCount -> Bool -> m [HypWithDesc t]
getLoopEqHypsAt tag split eqs visitNum useIfAt = do
    let details = SplitProofNodeDetails split 0 1 eqs
        mksub v = walkExprs $ \case
            Expr ty (ExprValueVar (Ident "%i")) | isMachineWordT ty -> v
            expr -> expr
        zsub = mksub (machineWordE 0)
        isub = mksub $ case fromJust (simpleVC visitNum) of
           SimpleVisitCountViewNumber n -> machineWordE n
           SimpleVisitCountViewOffset n -> machineWordVarE (Ident "%n") `plusE` machineWordE n
    visit <- getSplitVisitAt (WithTag tag details) visitNum
    start <- getSplitVisitAt (WithTag tag details) (numberVC 0)
    return $
        [ HypWithDesc (prettyTag tag ++ " pc imp") $ pcImpH (PcImpHypSidePc visit) (PcImpHypSidePc start)
        ] ++
        [ HypWithDesc (prettyTag tag ++ " const") $
            eqWithIfAtH useIfAt
                (eqSideH (zsub expr) start)
                (eqSideH (isub expr) visit)
                (Just (eqInductH split.unwrap 0))
        | Lambda { expr } <- eqs
        , instEqAtVisit expr visitNum
        ]
