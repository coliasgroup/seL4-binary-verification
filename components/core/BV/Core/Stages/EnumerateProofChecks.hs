{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Functor law" #-}

module BV.Core.Stages.EnumerateProofChecks
    ( enumerateProofChecks
    , pruneProofCheck
    ) where

import BV.Core.Logic (instEqAtVisit)
import BV.Core.Types
import BV.Core.Types.Extras

import Control.Monad (when)
import Control.Monad.Reader (MonadReader (..), ReaderT, runReader)
import Control.Monad.State (MonadState, StateT (StateT), evalStateT)
import Control.Monad.Writer (WriterT, execWriterT, mapWriterT, tell)
import Data.Foldable (for_, traverse_)
import Data.Function (applyWhen, on)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=), (.=))
import Text.Printf (printf)

type NodeProofChecks t = ProofCheckGroup t ProofCheckDescription

enumerateProofChecks :: RefineTag t => Problem t -> ByTag t FunctionSignature -> Pairing t -> ProofScript t () -> ProofScript t (NodeProofChecks t)
enumerateProofChecks problem sigs pairing proofScript =
    ProofScript $ runReader (evalStateT m initState) env
  where
    env = initEnv problem sigs pairing
    m = enumerateProofChecksInner
        (assumeR =<< instantiatePairingEqs PairingEqDirectionIn)
        proofScript.root

pruneProofCheck :: RefineTag t => ProblemAnalysis t -> ProofCheck t a -> ProofCheck t a
pruneProofCheck analysis = over checkVisits pruneVisitWithTag
  where
    pruneVisitWithTag (WithTag tag (Visit nodeId restrs)) =
        WithTag tag (Visit nodeId (filter (testRestr tag) restrs))
    testRestr tag (Restr nodeAddr _) = analysis.nodeTag nodeAddr == tag

data Env t
  = Env
      { problem :: Problem t
      , analysis :: ProblemAnalysis t
      , argRenames :: ArgRenames t
      , pairing :: Pairing t
      }
  deriving (Generic)

initEnv :: RefineTag t => Problem t -> ByTag t FunctionSignature -> Pairing t -> Env t
initEnv problem sigs pairing = Env
    { problem
    , analysis = analyzeProblem problem
    , argRenames = problemArgRenames problem sigs
    , pairing
    }

askPairing :: MonadReader (Env t) m => m (Pairing t)
askPairing = gview #pairing

askEntryPoints :: MonadReader (Env t) m => m (ByTag t NodeId)
askEntryPoints = gview $ #problem % #sides % to (fmap (.entryPoint))

askNodeGraph :: MonadReader (Env t) m => m NodeGraph
askNodeGraph = gview $ #analysis % #nodeGraph

askLoopHead :: (Tag t, MonadReader (Env t) m) => WithTag t NodeAddr -> m (Maybe (WithTag t NodeAddr))
askLoopHead n = fmap (WithTag n.tag) . loopHeadOf n.value <$> gview (#analysis % #loopData)

askLoopHeads :: (Tag t, MonadReader (Env t) m) => m [WithTag t NodeAddr]
askLoopHeads = do
    loopData <- gview $ #analysis % #loopData
    nodeTag <- gview $ #analysis % #nodeTag
    let withNodeTag n = WithTag (nodeTag n) n
    return $ map withNodeTag $ loopHeadsOf loopData

askArgRenames :: MonadReader (Env t) m => m (ArgRenames t)
askArgRenames = gview #argRenames

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

class (RefineTag t, MonadReader (Env t) m, MonadState (State t) m) => MonadChecks t m where
    branch :: m a -> m a
    branchRestrs :: m a -> m a

instance (RefineTag t, Monad m) => MonadChecks t (StateT (State t) (ReaderT (Env t) m)) where

    branch (StateT f) = StateT $ \s -> do
        (a, _) <- f s
        return (a, s)

    branchRestrs m = do
        restrs <- getRestrs
        a <- m
        #restrs .= restrs
        return a

type CheckWriter t = WriterT (NodeProofChecks t)

instance MonadChecks t m => MonadChecks t (CheckWriter t m) where
    branch = mapWriterT branch
    branchRestrs = mapWriterT branchRestrs

-- we provide functions for adding hyps and restrs to either side to enable
-- matching the behavior of graph-refine

assumeL :: MonadChecks t m => [Hyp t] -> m ()
assumeL hyps = #assumptions %= (hyps ++)

assumeR :: MonadChecks t m => [Hyp t] -> m ()
assumeR hyps = #assumptions %= (++ hyps)

assume1L :: MonadChecks t m => Hyp t -> m ()
assume1L = assumeL . (:[])

assume1R :: MonadChecks t m => Hyp t -> m ()
assume1R = assumeR . (:[])

restrictL :: MonadChecks t m => t -> [Restr] -> m ()
restrictL tag restrs = #restrs %= (map (WithTag tag) restrs ++)

restrictR :: MonadChecks t m => t -> [Restr] -> m ()
restrictR tag restrs = #restrs %= (++ map (WithTag tag) restrs)

restrict1L :: MonadChecks t m => t -> Restr -> m ()
restrict1L tag = restrictL tag . (:[])

restrict1R :: MonadChecks t m => t -> Restr -> m ()
restrict1R tag = restrictR tag . (:[])

conclude :: MonadChecks t m => ProofCheckDescription -> Hyp t -> CheckWriter t m ()
conclude meta hyp = do
    hyps <- use #assumptions
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

getRestrs :: MonadChecks t m => m [WithTag t Restr]
getRestrs = use #restrs

getRestrsForTag :: MonadChecks t m => t -> m [Restr]
getRestrsForTag _t = mapMaybe f <$> use #restrs
  where
    -- TODO HACK to match grap-refine
    -- f x = if x.tag == t then Just x.value else Nothing
    f x = Just x.value

getVisitWithTag :: MonadChecks t m => t -> NodeId -> m (WithTag t Visit)
getVisitWithTag tag n = WithTag tag . Visit n <$> getRestrsForTag tag

collect :: MonadChecks t m => CheckWriter t m () -> m (NodeProofChecks t)
collect = execWriterT

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
    renames <- askArgRenames
    let eqs = case direction of
            PairingEqDirectionIn -> pairing.inEqs
            PairingEqDirectionOut -> pairing.outEqs
    let eqSide side = do
            let tag = side.quadrant.tag
            visit <- case side.quadrant.direction of
                PairingEqDirectionIn -> do
                    entryPoint <- viewAtTag tag <$> askEntryPoints
                    return $ Visit entryPoint []
                PairingEqDirectionOut -> do
                    Visit Ret <$> getRestrsForTag tag
            let renamedExpr = renameVars (renames side.quadrant) side.expr
            return $ eqSideH renamedExpr (WithTag tag visit)
    for eqs $ \PairingEq { lhs, rhs } -> eqH <$> eqSide lhs <*> eqSide rhs

--

enumerateProofChecksInner
    :: MonadChecks t m
    => m ()
    -> ProofNodeWith t ()
    -> m (ProofNodeWith t (NodeProofChecks t))
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
    -- see note in checks.leaf_condition_checks
    retEq <- eqH
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
    let minPred = restrNode.range.x - 1
    let minPredVCOpt = case restrNode.range.kind of
            RestrProofNodeRangeKindOffset -> Just $ offsetVC (max 0 minPred)
            _ | minPred > 0 -> Just $ numberVC minPred
            _ -> Nothing
    for_ minPredVCOpt $ \minPredVC -> do
        pcTrueH <$> getVisitToRestrPointAfter restrNode minPredVC >>=
            conclude
                (printf "Check of restr min %d %P for %P"
                    restrNode.range.x
                    restrNode.range.kind
                    restrNode.point)
    pcFalseH <$> getVisitToRestrPointAfter restrNode (maxVCForRestrNode restrNode) >>=
        conclude
            (printf "Check of restr max %d %P for %P"
                restrNode.range.y
                restrNode.range.kind
                restrNode.point)

assumeRestrTriv :: MonadChecks t m => RestrProofNode t a -> m ()
assumeRestrTriv restrNode =
    assume1R =<< pcTrivH <$> getVisitToRestrPointAfter restrNode (maxVCForRestrNode restrNode)

applyRestrNodeRange :: MonadChecks t m => RestrProofNode t a -> m ()
applyRestrNodeRange restrNode = restrict1L restrNode.tag $
    Restr
        restrNode.point
        (foldMap
            (fromRestrKindVC restrNode.range.kind)
            [restrNode.range.x .. restrNode.range.y - 1])

applyRestrOthers :: MonadChecks t m => m ()
applyRestrOthers = getLoopsToSplit >>= traverse_ splitLoop
  where
    splitLoop (WithTag tag addr) = restrict1R tag (Restr addr (numbersVC [0, 1]))

getLoopsToSplit :: MonadChecks t m => m [WithTag t NodeAddr]
getLoopsToSplit = do
    restrs <- getRestrs
    loopHeadsWithSplit <- fmap catMaybes $ for restrs $ \restr -> askLoopHead (fmap (.nodeAddr) restr)
    loopHeads <- askLoopHeads
    let loopHeadsWithoutSplit = (S.difference `on` S.fromList) loopHeads loopHeadsWithSplit
    g <- askNodeGraph
    let pruneWith restr = applyWhen (not (hasZeroVC restr.value.visitCount)) $
            -- restr node must be visited, so loop heads must be  from restr (or on another tag)
            S.filter $ \loopHeadWithoutSplit ->
                loopHeadWithoutSplit.tag /= restr.tag
                    || isReachableFrom g (Addr restr.value.nodeAddr) (Addr loopHeadWithoutSplit.value)
    return $ S.toList $ foldr pruneWith loopHeadsWithoutSplit restrs

getVisitToRestrPointAfter :: MonadChecks t m => RestrProofNode t a -> VisitCount -> m (WithTag t Visit)
getVisitToRestrPointAfter restrNode vc = branchRestrs $ do
    restrict1L restrNode.tag $ Restr restrNode.point vc
    getVisitWithTag restrNode.tag (Addr restrNode.point)

maxVCForRestrNode :: RestrProofNode t a -> VisitCount
maxVCForRestrNode restrNode = fromRestrKindVC restrNode.range.kind (restrNode.range.y - 1)

--

emitSplitNodeChecks :: MonadChecks t m => SplitProofNode t () -> CheckWriter t m ()
emitSplitNodeChecks splitNode = branch $ do
    branch $ emitSplitNodeInitStepChecks splitNode
    branch $ emitSplitNodeInductStepChecks splitNode

emitSplitNodeInitStepChecks :: MonadChecks t m => SplitProofNode t () -> CheckWriter t m ()
emitSplitNodeInitStepChecks splitNode = branch $ do
    assume1L =<< getSplitNodeCErrHyp splitNode
    for_ [0 .. splitNode.n - 1] $ \i -> branch $ do
        visits <- getSplitVisitsAt (numberVC i) splitNode
        assume1R $ pcTrueH visits.left
        assume1R $ pcTrivH visits.right
        getSplitHypsAt (numberVC i) splitNode
            >>= concludeManyWith (\desc -> printf "Induct check at visit %d: %s" i desc)

emitSplitNodeInductStepChecks :: MonadChecks t m => SplitProofNode t () -> CheckWriter t m ()
emitSplitNodeInductStepChecks splitNode = branch $ do
    let n = splitNode.n
    conts <- getSplitVisitsAt (offsetVC n) splitNode
    assumeL [pcTrueH conts.left, pcTrivH conts.right]
    assume1L =<< getSplitNodeCErrHyp splitNode
    assumeSplitLoop splitNode False
    getSplitHypsAt (offsetVC n) splitNode
        >>= concludeManyWith (\desc ->
            printf "Induct check (%s) at inductive step for %P"
                desc
                splitNode.details.left.split)

assumeSplitNoLoop :: MonadChecks t m => SplitProofNode t () -> m ()
assumeSplitNoLoop splitNode = branchRestrs $ do
    visits <- getSplitVisitsAt (numberVC splitNode.n) splitNode
    assume1R $ pcFalseH visits.left

assumeSplitLoop :: MonadChecks t m => SplitProofNode t () -> Bool -> m ()
assumeSplitLoop splitNode exit = branchRestrs $ do
    let n = splitNode.n
    visits <- getSplitVisitsAt (offsetVC (n - 1)) splitNode
    conts <- getSplitVisitsAt (offsetVC n) splitNode
    assume1R $ pcTrueH visits.left
    when exit $ assume1R $ pcFalseH conts.left
    for_ [0 .. n - 1] $ \i ->
        assumeHyps =<< getSplitHypsAt (offsetVC i) splitNode

getSplitNodeCErrHyp :: MonadChecks t m => SplitProofNode t () -> m (Hyp t)
getSplitNodeCErrHyp splitNode =
    getSplitNodeCErrHypInner splitNode.n splitNode.loopRMax splitNode.details.right

getSplitNodeCErrHypInner :: MonadChecks t m => Integer -> Integer -> SplitProofNodeDetails -> m (Hyp t)
getSplitNodeCErrHypInner n loopRMax detailsR = branch $ do
    restrict1L rightTag $
        Restr detailsR.split $
            doubleRangeVC
                (detailsR.seqStart + (n * detailsR.step))
                (loopRMax + 2)
    applyRestrOthers
    pcFalseH <$> getVisitWithTag rightTag Err

getSplitVisitsAt :: MonadChecks t m => VisitCount -> SplitProofNode t () -> m (ByTag t (WithTag t Visit))
getSplitVisitsAt visit splitNode =
    traverse (getSplitVisitAt visit) (withTags splitNode.details)

getSplitVisitAt :: MonadChecks t m => VisitCount -> WithTag t SplitProofNodeDetails -> m (WithTag t Visit)
getSplitVisitAt visit (WithTag tag details) = branch $ do
    restrict1L tag $
        Restr details.split $
            case fromJust (simpleVC visit) of
                SimpleVisitCountViewOffset n ->
                    offsetVC $ n * details.step
                SimpleVisitCountViewNumber n ->
                    numberVC $ details.seqStart + (n * details.step)
    getVisitWithTag tag (Addr details.split)

getSplitHypsAt :: forall t m. MonadChecks t m => VisitCount -> SplitProofNode t () -> m [HypWithDesc t]
getSplitHypsAt vc splitNode = branch $ do
    visits <- getSplitVisitsAt vc splitNode
    starts <- getSplitVisitsAt (numberVC 0) splitNode
    let tagDesc tag = ((prettyTag (tag :: t) ++ " ") ++)
        imp = pcImpH `on` PcImpHypSidePc
        eq = eqInductH $ eqInductByTagH ((.split) <$> splitNode.details)
        inst = instEqAtVisit vc
        mksub v = walkExprs $ \case
            Expr ty (ExprValueVar (Ident "%i")) | isMachineWordT ty -> v
            expr -> expr
        zsub = mksub $ machineWordE 0
        isub = mksub $ case fromJust (simpleVC vc) of
            SimpleVisitCountViewNumber n -> machineWordE n
            SimpleVisitCountViewOffset n -> machineWordVarE (Ident "%n") `plusE` machineWordE n
    return $
        [ HypWithDesc "pc imp" $ visits.left `imp` visits.right
        , HypWithDesc (tagDesc leftTag "pc imp") $ visits.left `imp` starts.left
        , HypWithDesc (tagDesc rightTag "pc imp") $  visits.right `imp` starts.right
        ] ++
        [ HypWithDesc (tagDesc leftTag "const") $
            eq
                (eqSideH (zsub exprL) starts.left)
                (eqSideH (isub exprL) visits.left)
        | Lambda { expr = exprL } <- splitNode.details.left.eqs
        , inst exprL
        ] ++
        [ HypWithDesc (tagDesc rightTag "const") $
            eq
                (eqSideH (zsub exprR) starts.right)
                (eqSideH (isub exprR) visits.right)
        | Lambda { expr = exprR } <- splitNode.details.right.eqs
        , inst exprR
        ] ++
        [ HypWithDesc "eq" $
            eq
                (eqSideH (isub exprL) visits.left)
                (eqSideH (isub exprR) visits.right)
        | (Lambda { expr = exprL }, Lambda { expr = exprR }) <- splitNode.eqs
        , inst exprL && inst exprR
        ]

--

emitSingleRevInductNodeChecks :: MonadChecks t m => SingleRevInductProofNode t () -> CheckWriter t m ()
emitSingleRevInductNodeChecks node = branch $ do
    branch $ emitSingleLoopInductStepChecks node
    branch $ emitSingleLoopInductBaseChecks node
    branch $ emitSingleLoopRevInductChecks node
    branch $ emitSingleLoopRevInductBaseChecks node

emitSingleLoopInductStepChecks :: MonadChecks t m => SingleRevInductProofNode t () -> CheckWriter t m ()
emitSingleLoopInductStepChecks node = branch $ do
    let details = singleSplitDetails node
    assume1L =<< pcTrueH <$> getSplitVisitAt (offsetVC node.n) details
    for_ [0 .. node.n - 1] $ \i ->
        assumeHyps =<< getLoopEqHypsAt False (offsetVC i) node
    getLoopEqHypsAt False (offsetVC node.n) node
        >>= concludeManyWith
            (\desc ->
                printf "Induct check (%s) at inductive step for %P"
                    desc
                    node.point)

emitSingleLoopInductBaseChecks :: MonadChecks t m => SingleRevInductProofNode t () -> CheckWriter t m ()
emitSingleLoopInductBaseChecks node = branch $ do
    let details = singleSplitDetails node
    for_ [0 .. node.n] $ \i -> branch $ do
        assume1R =<< pcTrueH <$> getSplitVisitAt (numberVC i) details
        getLoopEqHypsAt False (numberVC i) node
            >>= concludeManyWith
                (\desc -> printf "Base check (%s, %d) at induct step for %P" desc i node.point)

emitSingleLoopRevInductChecks :: MonadChecks t m => SingleRevInductProofNode t () -> CheckWriter t m ()
emitSingleLoopRevInductChecks node = branch $ do
    let details = singleSplitDetails node
    curr <- getSplitVisitAt (offsetVC 1) details
    cont <- getSplitVisitAt (offsetVC 2) details
    assume1R $ pcTrueH curr
    assume1R $ trueIfAt node.pred_ cont
    assume1R =<< getSingleLoopRevCErrHyp details.value
    assumeHyps =<< getLoopEqHypsAt True (offsetVC 1) node
    conclude
        "Pred reverse step."
        (trueIfAt node.pred_ curr)

emitSingleLoopRevInductBaseChecks :: MonadChecks t m => SingleRevInductProofNode t () -> CheckWriter t m ()
emitSingleLoopRevInductBaseChecks node = branch $ do
    let details = singleSplitDetails node
    cont <- getSplitVisitAt (offsetVC 1) details
    assume1R =<< getLoopCounterEqHyp node
    assume1R $ pcTrueH cont
    assume1R =<< getSingleLoopRevCErrHyp details.value
    assumeHyps =<< getLoopEqHypsAt False (offsetVC 0) node
    conclude
        (printf "Pred true at %d check." node.nBound)
        (trueIfAt node.pred_ cont)

assumeSingleRevInduct :: MonadChecks t m => SingleRevInductProofNode t () -> m ()
assumeSingleRevInduct node = branchRestrs $ do
    restrict1R node.tag $ Restr node.point (numberVC 0)
    visit <- getVisitWithTag node.tag (Addr node.point)
    assume1R $ trueIfAt node.pred_ visit

getLoopCounterEqHyp :: MonadChecks t m => SingleRevInductProofNode t () -> m (Hyp t)
getLoopCounterEqHyp node = do
    let details = WithTag node.tag $ SplitProofNodeDetails node.point 0 1 []
    visit <- getSplitVisitAt (offsetVC 0) details
    return $
        eqInductH
            (eqInductSingleH node.point)
            (eqSideH (machineWordVarE (Ident "%n")) visit)
            (eqSideH (machineWordE node.nBound) visit)

getSingleLoopRevCErrHyp :: MonadChecks t m => SplitProofNodeDetails -> m (Hyp t)
getSingleLoopRevCErrHyp = getSplitNodeCErrHypInner 1 1

getLoopEqHypsAt :: MonadChecks t m => Bool -> VisitCount -> SingleRevInductProofNode t () -> m [HypWithDesc t]
getLoopEqHypsAt useIfAt vc node = do
    let details = singleSplitDetails node
    visit <- getSplitVisitAt vc details
    start <- getSplitVisitAt (numberVC 0) details
    let mksub v = walkExprs $ \case
            Expr ty (ExprValueVar (Ident "%i")) | isMachineWordT ty -> v
            expr -> expr
        zsub = mksub (machineWordE 0)
        isub = mksub $ case fromJust (simpleVC vc) of
           SimpleVisitCountViewNumber n -> machineWordE n
           SimpleVisitCountViewOffset n -> machineWordVarE (Ident "%n") `plusE` machineWordE n
    return $
        [ HypWithDesc (prettyTag node.tag ++ " pc imp") $ (pcImpH `on` PcImpHypSidePc) visit start
        ] ++
        [ HypWithDesc (prettyTag node.tag ++ " const") $
            (if useIfAt then eqIfAtInductH else eqInductH)
                (eqInductSingleH node.point)
                (eqSideH (zsub expr) start)
                (eqSideH (isub expr) visit)
        | Lambda { expr } <- node.eqs
        , instEqAtVisit vc expr
        ]

singleSplitDetails :: SingleRevInductProofNode t () -> WithTag t SplitProofNodeDetails
singleSplitDetails node = WithTag node.tag $ SplitProofNodeDetails node.point 0 1 node.eqs
