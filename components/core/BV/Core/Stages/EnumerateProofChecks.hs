module BV.Core.Stages.EnumerateProofChecks
    ( enumerateProofChecks
    ) where

import BV.Core.Graph
import BV.Core.Logic (instEqAtVisit)
import BV.Core.Types
import BV.Core.Types.Extras

import Control.Monad (when)
import Control.Monad.Reader (MonadReader (..), Reader, ReaderT, runReader)
import Control.Monad.State (MonadState, StateT (StateT), evalStateT)
import Control.Monad.Writer (WriterT, execWriterT, mapWriterT, tell)
import Data.Foldable (for_, traverse_)
import Data.Function (applyWhen, on)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
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
    m = enumerateProofChecksInner
        (assumeR =<< instantiatePairingEqs PairingEqDirectionIn)
        proofScript.root

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

data HypWithDesc
  = HypWithDesc
      { hyp :: Hyp
      , desc :: String
      }
  deriving (Generic)

concludeManyWith :: MonadChecks m => (String -> String) -> [HypWithDesc] -> CheckWriter m ()
concludeManyWith f hyps = for_ hyps $ \hyp -> conclude (f hyp.desc) hyp.hyp

assumeHyps :: MonadChecks m => [HypWithDesc] -> m ()
assumeHyps = assumeR . map (.hyp)

--

instantiatePairingEqs :: MonadChecks m => PairingEqDirection -> m [Hyp]
instantiatePairingEqs direction = branch $ do
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

enumerateProofChecksInner :: MonadChecks m => m () -> ProofNodeWith () -> m (ProofNodeWith NodeProofChecks)
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
    tag <- askNodeTag restrNode.point
    branchRestrs $ do
        applyRestrNodeRange restrNode
        applyRestrOthers
        assume1L =<< pcFalseH <$> getVisitWithTag C Err
    let visit vc = branchRestrs $ do
            restrict1L $ Restr restrNode.point vc
            getVisitWithTag tag $ Addr restrNode.point
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

assumeRestrTriv :: MonadChecks m => RestrProofNode a -> m ()
assumeRestrTriv restrNode = branchRestrs $ do
    applyRestrNodeMax restrNode
    assume1R =<< pcTrivH <$> getVisitWithTag restrNode.tag (Addr restrNode.point)

applyRestrNodeMax :: MonadChecks m => RestrProofNode a -> m ()
applyRestrNodeMax restrNode = restrict1L $
    Restr
        restrNode.point
        (maxVCForRestrNode restrNode.range)

maxVCForRestrNode :: RestrProofNodeRange -> VisitCount
maxVCForRestrNode range = fromRestrKindVC range.kind (range.y - 1)

applyRestrNodeRange :: MonadChecks m => RestrProofNode a -> m ()
applyRestrNodeRange restrNode = restrict1L $
    Restr
        restrNode.point
        (foldMap
            (fromRestrKindVC restrNode.range.kind)
            [restrNode.range.x .. restrNode.range.y - 1])

applyRestrOthers :: MonadChecks m => m ()
applyRestrOthers = do
    restrs <- getRestrs
    loopsToSplit <- askLoopsToSplit restrs
    restrictR [ Restr addr (numbersVC [0, 1]) | addr <- loopsToSplit ]
  where
    askLoopsToSplit restrs = do
        loopHeadsWithSplit <- fmap catMaybes $ for restrs $ \restr -> askLoopHead restr.nodeAddr
        loopHeads <- askLoopHeads
        let loopHeadsWithoutSplit = (S.difference `on` S.fromList) loopHeads loopHeadsWithSplit
        g <- askNodeGraph
        lookupNodeTag <- askLookupNodeTag
        let f restr = applyWhen (not (hasZeroVC restr.visitCount)) $ S.filter $ \loopHeadWithoutSplit ->
                isReachableFrom g (Addr restr.nodeAddr) (Addr loopHeadWithoutSplit)
                    || lookupNodeTag restr.nodeAddr /= lookupNodeTag loopHeadWithoutSplit
        return $ S.toList $ foldr f loopHeadsWithoutSplit (reverse restrs)

--

emitSplitNodeChecks :: MonadChecks m => SplitProofNode () -> CheckWriter m ()
emitSplitNodeChecks splitNode = branch $ do
    branch $ emitSplitNodeInitStepChecks splitNode
    branch $ emitSplitNodeInductStepChecks splitNode

emitSplitNodeInitStepChecks :: MonadChecks m => SplitProofNode () -> CheckWriter m ()
emitSplitNodeInitStepChecks splitNode = branch $ do
    assume1L =<< splitRErrPcHypM splitNode
    for_ [0 .. splitNode.n - 1] $ \i -> branch $ do
        visits <- getSplitVisitsAt splitNode (numberVC i)
        assumeR [pcTrueH visits.asm, pcTrivH visits.c]
        splitHypsAtVisit splitNode (numberVC i)
            >>= concludeManyWith (\desc -> printf "Induct check at visit %d: %s" i desc)

emitSplitNodeInductStepChecks :: MonadChecks m => SplitProofNode () -> CheckWriter m ()
emitSplitNodeInductStepChecks splitNode = branch $ do
    let n = splitNode.n
    errHyp <- splitRErrPcHypM splitNode
    conts <- getSplitVisitsAt splitNode (offsetVC n)
    assumeL [errHyp, pcTrueH conts.asm, pcTrivH conts.c]
    assumeSplitLoop splitNode False
    splitHypsAtVisit splitNode (offsetVC n)
        >>= concludeManyWith (\desc ->
            printf "Induct check (%s) at inductive step for %d"
                desc
                splitNode.details.asm.split)

splitRErrPcHypM :: MonadChecks m => SplitProofNode () -> m Hyp
splitRErrPcHypM splitNode = branch $ do
    restrict1L $
        Restr splitNode.details.c.split $
            doubleRangeVC
                (splitNode.details.c.seqStart + (splitNode.n * splitNode.details.c.step))
                (splitNode.loopRMax + 2)
    applyRestrOthers
    pcFalseH <$> getVisitWithTag C Err

assumeSplitNoLoop :: MonadChecks m => SplitProofNode () -> m ()
assumeSplitNoLoop splitNode = branchRestrs $ do
    visits <- getSplitVisitsAt splitNode (numberVC splitNode.n)
    assume1R $ pcFalseH visits.asm

getSplitVisitsAt :: MonadChecks m => SplitProofNode () -> VisitCount -> m (PairingOf VisitWithTag)
getSplitVisitsAt splitNode visit = for (withTags splitNode.details) $ \detailsWithTag ->
    getSplitVisitAt detailsWithTag visit

getSplitVisitAt :: MonadChecks m => WithTag SplitProofNodeDetails -> VisitCount -> m VisitWithTag
getSplitVisitAt (WithTag tag details) visit = branch $ do
    restrict1L $
        Restr details.split $
            case fromJust (simpleVC visit) of
                SimpleVisitCountViewOffset n ->
                    offsetVC $ n * details.step
                SimpleVisitCountViewNumber n ->
                    numberVC $ details.seqStart + (n * details.step)
    getVisitWithTag tag (Addr details.split)

assumeSplitLoop :: MonadChecks m => SplitProofNode () -> Bool -> m ()
assumeSplitLoop splitNode exit = branchRestrs $ do
    let n = splitNode.n
    visits <- getSplitVisitsAt splitNode (offsetVC (n - 1))
    conts <- getSplitVisitsAt splitNode (offsetVC n)
    assume1R $ pcTrueH visits.asm
    when exit $ assume1R $ pcFalseH conts.asm
    for_ [0 .. n - 1] $ \i ->
        assumeHyps =<< splitHypsAtVisit splitNode (offsetVC i)

splitHypsAtVisit :: MonadChecks m => SplitProofNode () -> VisitCount -> m [HypWithDesc]
splitHypsAtVisit splitNode visit = branch $ do
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
    let mk = flip HypWithDesc
        imp l r = pcImpH (PcImpHypSidePc l) (PcImpHypSidePc r)
        induct = Just $
            eqInductH
                splitNode.details.asm.split.unwrap
                splitNode.details.c.split.unwrap
    return $
        [ mk "pc imp" $ imp visits.asm visits.c
        , mk (prettyTag Asm ++ " pc imp") $ imp visits.asm starts.asm
        , mk (prettyTag C ++ " pc imp") $ imp visits.c starts.c
        ] ++
        [ mk (prettyTag Asm ++ " const") $
            eqH
                (eqSideH (zsub exprL) starts.asm)
                (eqSideH (lsub exprL) visits.asm)
                induct
        | Lambda { expr = exprL } <- splitNode.details.asm.eqs
        , inst exprL
        ] ++
        [ mk (prettyTag C ++ " const") $
            eqH
                (eqSideH (zsub exprR) starts.c)
                (eqSideH (lsub exprR) visits.c)
                induct
        | Lambda { expr = exprR } <- splitNode.details.c.eqs
        , inst exprR
        ] ++
        [ mk "eq" $
            eqH
                (eqSideH (lsub exprL) visits.asm)
                (eqSideH (lsub exprR) visits.c)
                induct
        | (Lambda { expr = exprL }, Lambda { expr = exprR }) <- splitNode.eqs
        , inst exprL && inst exprR
        ]

--

emitSingleRevInductNodeChecks :: MonadChecks m => SingleRevInductProofNode () -> CheckWriter m ()
emitSingleRevInductNodeChecks node = do
    tag <- askNodeTag node.point
    sequence_
        [ branch $ singleLoopInductStepChecksM node tag
        , branch $ singleLoopInductBaseChecksM node tag
        , branch $ singleLoopRevInductChecksM node tag
        , branch $ singleLoopRevInductBaseChecksM node tag
        ]

singleLoopInductStepChecksM :: MonadChecks m => SingleRevInductProofNode () -> Tag -> CheckWriter m ()
singleLoopInductStepChecksM node tag = do
    let eqsAssume = [] -- TODO
    let details = SplitProofNodeDetails node.point 0 1 node.eqs
    cont <- getSplitVisitAt (WithTag tag details) (offsetVC node.n)
    assume1L $ pcTrueH cont
    for_ [0.. node.n - 1] $ \i ->
        assumeHyps =<< loopEqHypsAtVisit tag node.point (eqsAssume ++ node.eqs) (offsetVC i) False
    loopEqHypsAtVisit tag node.point (eqsAssume ++ node.eqs) (offsetVC node.n) False
        >>= concludeManyWith (\desc ->
            printf "Induct check (%s) at inductive step for %d"
                desc
                node.point)

singleLoopInductBaseChecksM :: MonadChecks m => SingleRevInductProofNode () -> Tag -> CheckWriter m ()
singleLoopInductBaseChecksM node tag = do
    let details = SplitProofNodeDetails node.point 0 1 node.eqs
    for_ [0 .. node.n] $ \i -> do
        reach <- getSplitVisitAt (WithTag tag details) (numberVC i)
        branch $ do
            assume1R $ pcTrueH reach
            loopEqHypsAtVisit tag node.point node.eqs (numberVC i) False
                >>= concludeManyWith
                    (\desc -> printf "Base check (%s, %d) at induct step for %d" desc i node.point)

singleLoopRevInductChecksM :: MonadChecks m => SingleRevInductProofNode () -> Tag -> CheckWriter m ()
singleLoopRevInductChecksM node tag = do
    let eqsAssume = node.eqs
    let details = SplitProofNodeDetails node.point 0 1 eqsAssume
    curr <- getSplitVisitAt (WithTag tag details) (offsetVC 1)
    cont <- getSplitVisitAt (WithTag tag details) (offsetVC 2)
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
    assumeHyps =<< loopEqHypsAtVisit tag node.point eqsAssume (offsetVC 1) True
    let goal = trueIfAt' node.pred_ curr
    conclude
        "Pred reverse step."
        goal

singleLoopRevInductBaseChecksM :: MonadChecks m => SingleRevInductProofNode () -> Tag -> CheckWriter m ()
singleLoopRevInductBaseChecksM node tag = do
    let eqsAssume = node.eqs
    let details = SplitProofNodeDetails node.point 0 1 eqsAssume
    cont <- getSplitVisitAt (WithTag tag details) (offsetVC 1)
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
    assumeHyps =<< loopEqHypsAtVisit tag node.point eqsAssume (offsetVC 0) False
    let goal = trueIfAt' node.pred_ cont
    conclude
        (printf "Pred true at %d check." node.nBound)
        goal

mkLoopCounterEqHypM :: MonadChecks m => SingleRevInductProofNode () -> m Hyp
mkLoopCounterEqHypM node = do
    tag <- askNodeTag node.point
    let details = SplitProofNodeDetails node.point 0 1 []
    visit <- getSplitVisitAt (WithTag tag details) (offsetVC 0)
    return $
        eqH
            (eqSideH (machineWordVarE (Ident "%n")) visit)
            (eqSideH (machineWordE node.nBound) visit)
            (Just (eqInductH node.point.unwrap 0))

assumeSingleRevInduct :: MonadChecks m => SingleRevInductProofNode () -> m ()
assumeSingleRevInduct node = branchRestrs $ do
    restrict1R $ Restr node.point (numberVC 0)
    tag <- askNodeTag node.point
    vis <- getVisitWithTag tag (Addr node.point)
    assume1R $ trueIfAt' node.pred_ vis

loopEqHypsAtVisit :: MonadChecks m => Tag -> NodeAddr -> [Lambda] -> VisitCount -> Bool -> m [HypWithDesc]
loopEqHypsAtVisit tag split eqs visitNum useIfAt = do
    let details = SplitProofNodeDetails split 0 1 eqs
    visit <- getSplitVisitAt (WithTag tag details) visitNum
    start <- getSplitVisitAt (WithTag tag details) (numberVC 0)
    let mksub v = walkExprs $ \case
            Expr ty (ExprValueVar (Ident "%i")) | isMachineWordT ty -> v
            expr -> expr
    let zsub = mksub (machineWordE 0)
    let isub = mksub $ case fromJust (simpleVC visitNum) of
            SimpleVisitCountViewNumber n -> machineWordE n
            SimpleVisitCountViewOffset n -> machineWordVarE (Ident "%n") `plusE` machineWordE n
    let mk = flip HypWithDesc
    return $
        [ mk (prettyTag tag ++ " pc imp") $ pcImpH (PcImpHypSidePc visit) (PcImpHypSidePc start)
        ] ++
        [ mk (prettyTag tag ++ " const") $
            eqWithIfAtH useIfAt
                (eqSideH (zsub expr) start)
                (eqSideH (isub expr) visit)
                (Just (eqInductH split.unwrap 0))
        | Lambda { expr } <- eqs
        , instEqAtVisit expr visitNum
        ]
