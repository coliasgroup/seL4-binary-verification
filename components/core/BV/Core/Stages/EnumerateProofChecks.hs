module BV.Core.Stages.EnumerateProofChecks
    ( enumerateProofChecks
    ) where

import BV.Core.Graph
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils (optionals)

import Control.Monad.Reader (MonadReader (..), Reader, ReaderT, runReader)
import Control.Monad.State (MonadState, StateT (StateT), evalState, evalStateT)
import Control.Monad.Writer (WriterT (WriterT), execWriterT, mapWriterT, tell)
import Data.Foldable (traverse_)
import Data.Function (applyWhen, on)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid (Endo (Endo, appEndo))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))
import Text.Printf (printf)

type NodeProofChecks = [ProofCheck ProofCheckDescription]

enumerateProofChecks :: ArgRenames -> Pairing -> Problem -> ProofScript () -> ProofScript NodeProofChecks
enumerateProofChecks argRenames pairing problem proofScript =
    ProofScript $ runReader (evalStateT m initState) context
  where
    context = initContext argRenames pairing problem
    m = do
        initPointHypsM
        proofChecksRecM proofScript.root

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

askGetNodeTag :: MonadReader Context m => m (NodeAddr -> Tag)
askGetNodeTag = gview #nodeTag

data State
  = State
      { restrs :: [Restr]
      , hyps :: [Hyp]
      }
  deriving (Generic)

initState :: State
initState = State
    { restrs = []
    , hyps = []
    }

class (MonadReader Context m, MonadState State m) => MonadChecks m where
    branch :: m a -> m a

instance Monad m => MonadChecks (StateT State (ReaderT Context m)) where
    branch (StateT f) = StateT $ \s -> do
        (a, _) <- f s
        return (a, s)

type CheckWriter = WriterT NodeProofChecks

instance MonadChecks m => MonadChecks (CheckWriter m) where
    branch = mapWriterT branch

assumeL :: MonadChecks m => [Hyp] -> m ()
assumeL hyps = #hyps %= (hyps ++)

assumeR :: MonadChecks m => [Hyp] -> m ()
assumeR hyps = #hyps %= (++ hyps)

assume1L :: MonadChecks m => Hyp -> m ()
assume1L = assumeL . (:[])

assume1R :: MonadChecks m => Hyp -> m ()
assume1R = assumeR . (:[])

getAssumptions :: MonadChecks m => m [Hyp]
getAssumptions = use #hyps

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

liftReader :: MonadChecks m => Reader Context a -> m a
liftReader = reader . runReader

--

-- withAssumptionsR :: MonadChecks m => [Hyp] ->

proofChecksRecM :: MonadChecks m => ProofNodeWith () -> m (ProofNodeWith NodeProofChecks)
proofChecksRecM (ProofNodeWith _ node) = do
    case node of
        ProofNodeLeaf -> do
            checks <- collect $ branch leafChecksM
            return $ ProofNodeWith checks ProofNodeLeaf
        ProofNodeRestr restrNode -> do
            checks <- collect $ branch $ restrChecksM restrNode
            do
                restrs <- getRestrs
                let restr = Restr
                        restrNode.point
                        (fromRestrKindVC restrNode.range.kind (restrNode.range.y - 1))
                assume1R $ pcTrivH
                        (tagV restrNode.tag
                            (Visit (Addr restrNode.point) (restr:restrs)))
            restrict1L $ getProofRestr restrNode.point restrNode.range
            ProofNodeWith checks . ProofNodeRestr <$>
                traverseRestrProofNodeChild
                    proofChecksRecM
                    restrNode
        ProofNodeCaseSplit caseSplitNode -> do
            visit <- tagV caseSplitNode.tag <$> getVisit (Addr caseSplitNode.addr)
            ProofNodeWith [] . ProofNodeCaseSplit <$>
                traverseCaseSplitProofNodeChildren
                    (goWith [pcTrueH visit])
                    (goWith [pcFalseH visit])
                    caseSplitNode
        ProofNodeSplit splitNode -> do
            restrs <- getRestrs
            hyps <- getAssumptions
            checks <- liftReader $ splitChecksM restrs hyps splitNode
            ProofNodeWith checks . ProofNodeSplit <$>
                traverseSplitProofNodeChildren
                    (goWith (splitNoLoopHyps splitNode restrs))
                    (goWith (splitLoopHyps splitNode restrs True))
                    splitNode
        ProofNodeSingleRevInduct singleRevInductNode -> do
            restrs <- getRestrs
            hyps <- getAssumptions
            checks <- liftReader $ singleRevInductChecksM restrs hyps singleRevInductNode
            hyp' <- liftReader $ singleRevInductResultingHypM restrs singleRevInductNode
            assume1R hyp'
            ProofNodeWith checks . ProofNodeSingleRevInduct <$>
                traverseSingleRevInductProofNodeChild
                    proofChecksRecM
                    singleRevInductNode
  where
    goWith hyps n = branch $ do
        assumeR hyps
        proofChecksRecM n

leafChecksM :: MonadChecks m => CheckWriter m ()
leafChecksM = do
    -- nonRErrPcH <$> getRestrs >>= assume1L
    nerrPcHyp <- nonRErrPcH <$> getRestrs
    nlerrPc <- pcFalseH . asmV <$> getVisit Err
    retEq <- eqH'
        <$> (eqSideH trueE . asmV <$> getVisit Ret)
        <*> (eqSideH trueE . cV <$> getVisit Ret)
    assume1L nerrPcHyp
    instEqs <- instEqsM PairingEqDirectionOut
    concludeWith "Leaf path-cond imp" [retEq] nlerrPc
    traverse_ (concludeWith "Leaf eq check" [nlerrPc, retEq]) instEqs

restrChecksM :: MonadChecks m => RestrProofNode () -> CheckWriter m ()
restrChecksM restrNode = do
    restrs <- getRestrs
    let restr = getProofRestr restrNode.point restrNode.range
    restrOthers <- liftReader $ restrOthersM (restr : restrs) 2
    let nCErrHyp = nonRErrPcH restrOthers
    assume1L nCErrHyp
    tag <- askNodeTag restrNode.point
    let visit vc = tagV tag
            (Visit (Addr restrNode.point) (Restr restrNode.point vc : restrs))
    let minVC = case restrNode.range.kind of
            RestrProofNodeRangeKindOffset -> Just $ offsetVC (max 0 (restrNode.range.x - 1))
            _ | restrNode.range.x > 1 -> Just $ numberVC (restrNode.range.x - 1)
            _ -> Nothing
    case minVC of
            Just minVC' -> conclude
                (printf "Check of restr min %d %s for %d" restrNode.range.x (prettyRestrProofNodeRangeKind restrNode.range.kind) restrNode.point.unwrap)
                (pcTrueH (visit minVC'))
            Nothing -> return ()
    let topVC = fromRestrKindVC restrNode.range.kind (restrNode.range.y - 1)
    conclude
        (printf "Check of restr max %d %s for %d" restrNode.range.y (prettyRestrProofNodeRangeKind restrNode.range.kind) restrNode.point.unwrap)
        (pcFalseH (visit topVC))

restrOthersM :: [Restr] -> Integer -> Reader Context [Restr]
restrOthersM restrs n = do
    xs <- loopsToSplitM restrs
    let extras = [ Restr sp (upToVC n) | sp <- xs ]
    return $ restrs ++ extras

loopsToSplitM :: [Restr] -> Reader Context [NodeAddr]
loopsToSplitM restrs = do
    loopHeadsWithSplit <- fmap (S.fromList . catMaybes) . for restrs $ \restr -> askLoopHead restr.nodeAddr
    loopHeads_ <- S.fromList <$> askLoopHeads
    let remLoopHeadsInit = loopHeads_ `S.difference` loopHeadsWithSplit
    g <- askNodeGraph
    nodeTag <- askGetNodeTag
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
    let visit' = case fromJust (simpleVC visit) of
            SimpleVisitCountViewOffset n -> offsetVC (n * detailsWithTag.value.step)
            SimpleVisitCountViewNumber n -> numberVC (detailsWithTag.value.seqStart + (n * detailsWithTag.value.step))
        restr' = Restr detailsWithTag.value.split visit'
     in Visit (Addr detailsWithTag.value.split) (restr' : restrs)

splitHypsAtVisit :: SplitProofNode () -> [Restr] -> VisitCount -> [(Hyp, ProofCheckDescription)]
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
    mksub v = walkExprs $ \case
        Expr ty (ExprValueVar (Ident "%i")) | isMachineWordT ty -> v
        expr -> expr
    inst expr = instEqAtVisit expr visit
    zsub = mksub (machineWordE 0)
    lsub = mksub $ case fromJust (simpleVC visit) of
        SimpleVisitCountViewNumber n -> machineWordE n
        SimpleVisitCountViewOffset n -> machineWordVarE (Ident "%n") `plusE` machineWordE n

loopEqHypsAtVisit :: Tag -> NodeAddr -> [Lambda] -> [Restr] -> VisitCount -> Bool -> [(Hyp, ProofCheckDescription)]
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
    mksub v = walkExprs $ \case
        Expr ty (ExprValueVar (Ident "%i")) | isMachineWordT ty -> v
        expr -> expr
    zsub = mksub (machineWordE 0)
    isub = mksub $ case fromJust (simpleVC visitNum) of
        SimpleVisitCountViewNumber n -> machineWordE n
        SimpleVisitCountViewOffset n -> machineWordVarE (Ident "%n") `plusE` machineWordE n

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

splitLoopHyps :: SplitProofNode () -> [Restr] -> Bool -> [Hyp]
splitLoopHyps splitNode restrs exit =
    [lEnter] ++ optionals exit [lExit] ++
        [ hyp
        | offs <- [ offsetVC i | i <- [0..n-1] ]
        , (hyp, _) <- splitHypsAtVisit splitNode restrs offs
        ]
  where
    n = splitNode.n
    visits = splitVisitVisits splitNode restrs (offsetVC (n - 1))
    conts = splitVisitVisits splitNode restrs (offsetVC n)
    lEnter = pcTrueH visits.asm
    lExit = pcFalseH conts.asm

singleRevInductChecksM :: [Restr] -> [Hyp] -> SingleRevInductProofNode () -> Reader Context NodeProofChecks
singleRevInductChecksM restrs hyps node = do
    tag <- askNodeTag node.point
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
    tag <- askNodeTag node.point
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
    tag <- askNodeTag node.point
    let vis = tagV tag $ Visit (Addr node.point) $ restrs ++ [Restr node.point (numberVC 0)]
    return $ trueIfAt' node.pred_ vis

--

initPointHypsM :: MonadChecks m => m ()
initPointHypsM = do
    hyps <- instEqsM PairingEqDirectionIn
    assumeR hyps

instEqsM :: MonadChecks m => PairingEqDirection -> m [Hyp]
instEqsM direction = do
    pairing <- askPairing
    let eqs = case direction of
            PairingEqDirectionIn -> pairing.inEqs
            PairingEqDirectionOut -> pairing.outEqs
    entryPoints <- askEntryPoints
    restrs <- getRestrs
    let addrMap quadrant = tagV quadrant.tag $ case quadrant.direction of
            PairingEqDirectionIn -> Visit (pairingSide quadrant.tag entryPoints) []
            PairingEqDirectionOut -> Visit Ret restrs
    renames <- askArgRenames
    return
        [ let f eqSide = eqSideH
                (renameVarsI (renames eqSide.quadrant) eqSide.expr)
                (addrMap eqSide.quadrant)
           in (eqH' `on` f) lhs rhs
        | PairingEq { lhs, rhs } <- eqs
        ]

--

nonRErrPcH :: [Restr] -> Hyp
nonRErrPcH restrs = pcFalseH . cV $ Visit Err restrs
