{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.GraphSlice.Old.Core
    ( FunCallInfo (..)
    , GraphSliceHooks
    , GraphSliceT
    , TooGeneral (..)
    , VarRepRequestKind (..)
    , VarReqRequest (..)
    , askProblemWithAnalysis
    , asmRefineGraphSliceHooks
    , defaultGraphSliceHooks
    , flattenExpr
    , getAllPcEnvs
    , getCallOrderCompat
    , getCallsByFunName
    , getInductVar
    , getPc
    , getPcEnv
    , getPcEnvTagged
    , getPcTagged
    , getSuccessVar
    , instEqWithEnvs
    , isVisitOk
    , runGraphSliceTStep
    , withAsmStackSplitting
    , withConstRetAssumptions
    , withFast
    ) where

import BV.Core.GraphSlice.Old.NameHint
import BV.Core.GraphSlice.Old.Solver

import BV.Core.GraphSlice.New (FlatExpr)
import BV.Core.GraphSlice.New.Common (MonadGraphSliceSendSExpr,
                                      MonadLiftInner (..),
                                      MonadMapInnermost (..))
import BV.Core.GraphSlice.New.Flatten.CallCount
import BV.Core.GraphSlice.New.Flatten.VisitInfo

import BV.Core.GenerateFreshName (generateFreshName)
import BV.Core.Logic
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils
import BV.Utils

import Control.Monad (filterM, guard, when, (>=>))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (IdentityT (runIdentityT), runIdentity)
import Control.Monad.Reader (Reader, ReaderT (runReaderT), mapReaderT)
import Control.Monad.RWS (MonadState (get))
import Control.Monad.State (StateT, evalStateT, execStateT, mapStateT, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), hoistMaybe, runMaybeT)
import Data.Either (isRight)
import Data.Foldable (for_, toList, traverse_)
import Data.Functor (void)
import Data.List (genericIndex, isPrefixOf)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))
import Text.Printf (printf)

type T = GraphSliceT

type InnerT = GraphSliceSolverT

type C t m = (Tag t, MonadGraphSliceSendSExpr m)

type RefineC t m = (C t m, RefineTag t)

type AsmRefineC t m = (C t m, t ~ AsmRefineTag)

--

newtype GraphSliceT t m a
  = GraphSliceT { run :: StateT (TState t) (ReaderT (TEnv t) (InnerT m)) a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance MonadTrans (T t) where
    lift = liftInner . lift

instance MonadLiftInner InnerT (T t) where
    liftInner = GraphSliceT . lift . lift

instance MonadMapInnermost (T t) where
    mapInnermost f = #run %~ mapStateT (mapReaderT (mapInnermost f))

runGraphSliceTStep
    :: (Tag t, MonadGraphSliceSendSExpr m)
    => ProblemWithAnalysis t
    -> GraphSliceHooks t
    -> T t m a
    -> InnerT m a
runGraphSliceTStep pwa hooks m =
      flip runReaderT (initEnv pwa hooks)
    . flip evalStateT initState
    . (.run)
    $ m'
  where
    m' = do
        initGraphSlice
        m

data TEnv t
  = TEnv
      { pwa :: ProblemWithAnalysis t
      , varNames :: S.Set Ident
      , hooks :: GraphSliceHooks t
      }
  deriving (Generic)

-- TODO abuse of PairingEqDirection
data GraphSliceHooks t
  = GraphSliceHooks
      { isStackHook :: VarRepRequestKind -> WithTag t NameTy -> Maybe VarReqRequest
      , addFunAsserts :: AddFunAssertsHook t
      , fast :: Bool
      , constRetAssumptions :: WithTag t Ident -> Integer -> Maybe Integer
      }
  deriving (Generic)

type AddFunAssertsHookFn t = forall m. MonadGraphSliceSendSExpr m => Visit t -> T t m ()

newtype AddFunAssertsHook t
  = AddFunAssertsHook (AddFunAssertsHookFn t)

data TState t
  = TState
      { inpEnvs :: Map NodeId ExprEnv
      , memCalls :: Map SmtName CallCount
      , nodePcEnvs :: Map (Visit t) (Maybe PcEnv)
      , arcPcEnvs :: Map (Visit t) (Map NodeId PcEnv)
      , inductVarEnv :: Map EqHypInduct SmtName
      , condVars :: Map MaybeSplit Ident
      , contractions :: Map SExprWithPlaceholders FlatExpr
      , extraProblemNames :: S.Set Ident
      , funCalls :: M.Map (Visit t) FunCallInfo
      , funCallsByName :: M.Map (WithTag t Ident) [Visit t]
      , funCallOrder :: Seq.Seq (Visit t)
      }
  deriving (Generic)

data FunCallInfo
  = FunCallInfo
      { ins :: [FlatExpr]
      , outs :: [FlatExpr]
      , success :: FlatExpr
      }
  deriving (Eq, Generic, Ord, Show)

initEnv :: Tag t => ProblemWithAnalysis t -> GraphSliceHooks t -> TEnv t
initEnv pwa hooks = TEnv
    { pwa
    , varNames = S.fromList $ toListOf varNamesOfProblem pwa.problem
    , hooks
    }

defaultGraphSliceHooks :: GraphSliceHooks t
defaultGraphSliceHooks = GraphSliceHooks
    { isStackHook = \_ _ -> Nothing
    , addFunAsserts = AddFunAssertsHook $ \_ -> return ()
    , fast = False
    , constRetAssumptions = \_ _ -> Nothing
    }

asmRefineGraphSliceHooks
    :: t ~ AsmRefineTag
    => LookupFunctionSignature t
    -> Pairings t
    -> ArgRenames t
    -> GraphSliceHooks t
asmRefineGraphSliceHooks lookupSig pairings argRenames =
    withAsmStackSplitting lookupSig argRenames $
        defaultGraphSliceHooks
            & #addFunAsserts .~ addFunAssertsHook lookupSig pairings

withAsmStackSplitting
    :: HasTagIsAsm t
    => LookupFunctionSignature t
    -> ArgRenames t
    -> GraphSliceHooks t
    -> GraphSliceHooks t
withAsmStackSplitting _lookupSig argRenames =
    #isStackHook .~ asmRefineIsStackHook argRenames

withConstRetAssumptions :: (WithTag t Ident -> Integer -> Maybe Integer) -> GraphSliceHooks t -> GraphSliceHooks t
withConstRetAssumptions constRetAssumptions = #constRetAssumptions .~ constRetAssumptions

withFast :: GraphSliceHooks t -> GraphSliceHooks t
withFast = #fast .~ True

initState :: TState t
initState = TState
    { inpEnvs = M.empty
    , memCalls = M.empty
    , nodePcEnvs = M.empty
    , arcPcEnvs = M.empty
    , inductVarEnv = M.empty
    , condVars = M.empty
    , contractions = M.empty
    , extraProblemNames = S.empty
    , funCalls = M.empty
    , funCallsByName = M.empty
    , funCallOrder = Seq.empty
    }

--

getCallsByFunName :: (Tag t, Monad m) => T t m (M.Map (WithTag t Ident) [Visit t])
getCallsByFunName = liftPure $ use #funCallsByName

getCallOrderCompat :: Monad m => T t m [Visit t]
getCallOrderCompat = liftPure $ do
    funCallOrder <- use #funCallOrder
    return $ toList funCallOrder

getSuccessVar :: C t m => Visit t -> T t m FlatExpr
getSuccessVar visit = (.success) <$> getFunCallInfo visit

--

liftFlat :: Monad m => InnerT m a -> T t m a
liftFlat = liftInner

liftPure :: Monad m => StateT (TState t) (Reader (TEnv t)) a -> T t m a
liftPure = GraphSliceT . mapStateT (mapReaderT (return . runIdentity))

initGraphSlice :: C t m => T t m ()
initGraphSlice = do
    addInputEnvs

--

withMapSlotWithMapping :: (C t m, Monad m', Ord k) => (forall a. T t m a -> m' a) -> Lens' (TState t) (M.Map k v) -> k -> m' v -> m' v
withMapSlotWithMapping f = withMapSlotWith $ f . liftPure . mapStateT (return . runIdentity)

withMapSlot :: (C t m, Ord k) => Lens' (TState t) (Map k v) -> k -> T t m v -> T t m v
withMapSlot = withMapSlotWithMapping id

applyTag :: Visit t -> a -> WithTag t a
applyTag visit = WithTag visit.tag

--

askProblem :: C t m => T t m (Problem t)
askProblem = (.problem) <$> askProblemWithAnalysis

askProblemWithAnalysis :: (Tag t, Monad m) => T t m (ProblemWithAnalysis t)
askProblemWithAnalysis = liftPure $ gview #pwa

askProblemSide :: (Tag t, Monad m) => t -> T t m ProblemSide
askProblemSide tag = do
    p <- askProblemWithAnalysis
    return $ viewAtTag tag p.problem.sides

askProblemSideWithAnalysis :: (Tag t, Monad m) => t -> T t m ProblemSideWithAnalysis
askProblemSideWithAnalysis tag = problemSideWithAnalysis tag <$> askProblemWithAnalysis

askHook :: C t m => Lens' (GraphSliceHooks t) a -> T t m a
askHook l = liftPure $ gview $ #hooks % l

askNode :: C t m => Visit t -> T t m Node
askNode visit = do
    side <- askProblemSide visit.tag
    return $ side.nodes ! nodeAddrOf visit.nodeId

askFunName :: C t m => Visit t -> T t m Ident
askFunName v = view (expecting #_NodeCall % #functionName) <$> askNode v

askPreds :: C t m => Visit t -> T t m (Set NodeAddr)
askPreds visit = do
    side <- askProblemSideWithAnalysis visit.tag
    return $ side.analysis.preds visit.nodeId

askPredVisits :: C t m => Visit t -> T t m [Visit t]
askPredVisits visit = do
    side <- askProblemSideWithAnalysis visit.tag
    return $ predVisits visit (toList (side.analysis.preds visit.nodeId))

askContVisits :: C t m => Visit t -> T t m [Visit t]
askContVisits visit = do
    node <- askNode visit
    return $ contVisits visit (toListOf nodeConts node)

askContVisit :: C t m => Visit t -> T t m (Visit t)
askContVisit visit = do
    conts <- askContVisits visit
    let [cont] = conts
    return cont

--

askNonConstOutputs :: C t m => t -> CallNode -> T t m [NameTy]
askNonConstOutputs tag callNode = do
    fast <- askHook #fast
    constRetAssumptions <- askHook #constRetAssumptions
    return $
        if not fast
        then callNode.output
        else
            [ out
            | (i, out) <- zip [0..] callNode.output
            , not $ case constRetAssumptions (WithTag tag callNode.functionName) i of
                    Just j -> callNode.input `genericIndex` j == varFromNameTyE out
                    Nothing -> False
            ]

--

getFreshIdent :: C t m => NameHint -> T t m Ident
getFreshIdent nameHint = do
    problemNames <- liftPure $ gview $ #varNames
    extraProblemNames <- liftPure $ use #extraProblemNames
    let taken n = S.member n problemNames || S.member n extraProblemNames
    let n = Ident $ generateFreshName (taken . Ident) nameHint
    liftPure $ #extraProblemNames %= S.insert n
    return n

maybeContract :: C t m => Visit t -> Ident -> FlatExpr -> T t m FlatExpr
maybeContract visit name expr@(Expr ty (ExprValueSMTExpr ms)) = case ms of
    NotSplit sexpr | compareLength 80 (showSExprWithPlaceholders sexpr) == GT -> withMapSlot #contractions sexpr $ do
        let name' = localNameBefore visit name
        liftFlat $ smtExprE ty <$> addDef name' (smtExprE ty (NotSplit sexpr))
    _ -> return expr

contractPcEnv :: C t m => Visit t -> PcEnv -> T t m PcEnv
contractPcEnv visit (PcEnv pc env) = do
    pc' <- case pc.value of
        ExprValueSMTExpr _ -> return pc
        _ -> do
            let hint = pathCondName visit
            name <- liftFlat $ addDef hint pc
            return $ smtExprE boolT name
    env' <- M.traverseWithKey (maybeContract visit) env
    return $ PcEnv pc' env'

flattenExpr :: ExprEnv -> GraphExpr -> FlatExpr
flattenExpr = flip go
  where
    go = traverseOf (exprArgs % traversed) go >=> \expr -> case expr.value of
        ExprValueVar name -> (! name)
        _ -> return expr

flattenAndAddDef :: C t m => ExprEnv -> NameHint -> GraphExpr -> T t m MaybeSplit
flattenAndAddDef env nameHint val = liftFlat $ addDef nameHint $ flattenExpr env val

--

getMemCalls :: C t m => SExprWithPlaceholders -> T t m CallCount
getMemCalls = liftFlat . getImmBasisMems >=> \mems -> fmap (foldr1 mergeCallCounts) $ for (S.toList mems) $ \mem ->
    liftPure $ use $ #memCalls % expectingAt mem

scanMemCallsEnv :: C t m => ExprEnv -> T t m MemCallsIfKnown
scanMemCallsEnv = scanMemCalls . toList

scanMemCalls :: C t m => [FlatExpr] -> T t m MemCallsIfKnown
scanMemCalls tyVals = do
    memCalls <- traverse getMemCalls [ v | Expr ty (ExprValueSMTExpr (NotSplit v)) <- tyVals, ty == memT ]
    return $ case memCalls of
        [] -> Nothing
        _ -> Just $ foldr1 mergeCallCounts memCalls

--

pruneVisit :: C t m => Visit t -> T t m (Maybe (Visit t))
pruneVisit visit = do
    side <- askProblemSideWithAnalysis visit.tag
    runMaybeT $
        forOf #restrs visit $ \restrs ->
            fmap (M.fromList . concat) $ for (M.toList restrs) $ \(addr, vc) ->
                if addr `M.notMember` side.problem.nodes
                    then return []
                    else do
                        let reachable = isNonTriviallyReachableFrom side addr visit.nodeId
                        guard $ reachable || hasZeroVC vc
                        return [ (addr, vc) | reachable ]

pruneVisits :: C t m => [Visit t] -> T t m [Visit t]
pruneVisits visits = catMaybes <$> traverse pruneVisit visits

data TooGeneral
  = TooGeneral
      { split :: NodeAddr
      }
  deriving (Eq, Generic, Ord, Show)

checkGenerality :: C t m => Visit t -> ExceptT TooGeneral (T t m) ()
checkGenerality visit = void $ runMaybeT $ do
    side <- lift $ lift $ askProblemSideWithAnalysis visit.tag
    let loopData = side.analysis.loopData
    nodeAddr <- hoistMaybe $ preview #_Addr visit.nodeId
    loop <- hoistMaybe $ outermostLoopContaining loopData nodeAddr
    ifor_ visit.restrs $ \addr vc -> do
        let loopOpt' = outermostLoopContaining loopData addr
        when (fmap (.head) loopOpt' == Just loop.head && isOptionsVC vc) $ do
            throwError $ TooGeneral { split = addr }

--

data VarRepRequestKind
  = VarRepRequestKindInit
  | VarRepRequestKindLoop
  | VarRepRequestKindCall
  deriving (Eq, Generic, Ord, Show)

data VarReqRequest
  = VarRepRequestSplitMem
      { addr :: GraphExpr
      }
  deriving (Eq, Generic, Ord, Show)

type MemCallsIfKnown = Maybe CallCount

-- TODO rename?
addVarReps
    :: C t m
    => VarRepRequestKind
    -> (Ident -> NameHint)
    -> MemCallsIfKnown
    -> Visit t
    -> [NameTy]
    -> ExprEnv
    -> T t m ExprEnv
addVarReps kind mkName memCalls visit vars = execStateT $ do
    for_ vars $ \var -> do
        v <- lift $ smtExprE var.ty . NotSplit . nameS <$> addVarWithMemCalls (mkName var.name) var.ty memCalls
        modify $ M.insert var.name v
    intermediateEnv <- get
    for_ vars $ \var -> do
        opt <- lift $ varRepRequest kind visit intermediateEnv var
        for_ opt $ \splitMem -> modify $ M.insert var.name $ smtExprE var.ty $ Split splitMem

addVarWithMemCalls :: C t m => NameHint -> ExprType -> MemCallsIfKnown -> T t m SmtName
addVarWithMemCalls nameHint ty memCallsOpt = do
    v <- liftFlat $ addVar nameHint ty
    when (isMemT ty) $ do
        liftPure $ #memCalls %= M.insert v (fromJust memCallsOpt)
    return v

varRepRequest :: C t m => VarRepRequestKind -> Visit t -> ExprEnv -> NameTy -> T t m (Maybe SplitMem)
varRepRequest kind visit env var = do
    isStackHook <- askHook #isStackHook
    let reqOpt = isStackHook kind $ applyTag visit var
    for reqOpt $ \req -> case req of
        VarRepRequestSplitMem { addr } -> do
            addrSExpr <- liftFlat $ convertExprNotSplit $ flattenExpr env addr
            let nameHint = printf "%P_for_%s" var.name (nodeCountName visit)
            liftFlat $ addSplitMemVar addrSExpr nameHint var.ty

--

-- HACK
updatePcEnvCompat :: C t m => PcEnv -> T t m PcEnv
updatePcEnvCompat pcEnv = traverseOf #pc (walkExprsM f) pcEnv
  where
    f expr = case expr.value of
        ExprValueSMTExpr s -> do
            condIdentOpt <- liftPure $ use $ #condVars % at s
            return $ case condIdentOpt of
                Just condIdent -> flattenExpr pcEnv.env (varE boolT condIdent)
                Nothing -> expr
        _ -> return expr

--

getInductVar :: C t m => EqHypInduct -> T t m FlatExpr
getInductVar induct =
    fmap (smtExprE ty . NotSplit . nameS) $
        withMapSlot #inductVarEnv induct $
            liftFlat $ addVar (printf "induct_i_%d_%d" induct.n1 induct.n2) ty
  where
    ty = word32T

getPcEnv :: (Tag t, MonadGraphSliceSendSExpr m) => Visit t -> GraphSliceT t m (Maybe PcEnv)
getPcEnv visit = getPcEnvTagged visit

getPc :: (Tag t, MonadGraphSliceSendSExpr m) => Visit t -> GraphSliceT t m FlatExpr
getPc visit = getPcEnv visit <&> \case
    Just (PcEnv pc _) -> pc
    Nothing -> falseE

getPcTagged :: C t m => Visit t -> T t m FlatExpr
getPcTagged visit = getPcEnvTagged visit >>= \case
    Nothing -> return falseE
    Just (PcEnv pc _) -> liftFlat $ convertInnerExpr pc

getPcEnvTagged :: C t m => Visit t -> T t m (Maybe PcEnv)
getPcEnvTagged = runIdentityT . getPcEnvTaggedInner (const (return ()))

isVisitOk :: C t m => Visit t -> T t m Bool
isVisitOk visit = isRight <$> runExceptT (checkGenerality visit)

getPcEnvTaggedInner :: (C t m, MonadTrans trans) => (Visit t -> trans (T t m) ()) -> Visit t -> trans (T t m) (Maybe PcEnv)
getPcEnvTaggedInner check unprunedVisit = runMaybeT $ do
    visit <- MaybeT $ lift $ pruneVisit unprunedVisit
    lift $ check visit
    MaybeT $ lift $ withMapSlot #nodePcEnvs visit $ do
        warmPcEnvCache visit
        getPcEnvTaggedRaw visit

getPcEnvTaggedRaw :: C t m => Visit t -> T t m (Maybe PcEnv)
getPcEnvTaggedRaw visit = do
    liftPure (use $ #inpEnvs % at visit.nodeId) >>= \case
        Just env -> return $ Just $ PcEnv trueE env
        Nothing -> do
            let f (addr, vc) = Addr addr == visit.nodeId && vc == offsetVC 0
            if any f (M.toList visit.restrs)
                then getLoopPcEnv visit
                else do
                    arcPcEnvs <- toListOf (folded % folded) <$> do
                        preds <- toList <$> askPreds visit
                        for preds $ \pred_ -> getArcPcEnvs pred_ visit
                    case arcPcEnvs of
                        [] -> return Nothing
                        _ -> Just <$> do
                            let optimize = case visit.nodeId of
                                    Err -> traverse $ \(PcEnv pc _) -> do
                                        pc' <- liftFlat $ convertInnerExpr pc
                                        return $ PcEnv pc' M.empty
                                    _ -> return
                            optimizedArcPcEnvs <- optimize arcPcEnvs
                            (pcEnv, _large) <- liftFlat $ mergeEnvsPcs optimizedArcPcEnvs
                            updatePcEnvCompat pcEnv >>= contractPcEnv visit

addInputEnvs :: C t m => T t m ()
addInputEnvs = do
    p <- askProblem
    traverse_ f (withTags p.sides)
  where
    f (WithTag tag side) = do
        env <- addVarReps
            VarRepRequestKindInit
            (\name -> name.unwrap ++ "_init")
            (Just emptyCallCount)
            (Visit tag side.entryPoint M.empty)
            side.input
            M.empty
        liftPure $ #inpEnvs %= M.insert side.entryPoint env


getLoopPcEnv :: C t m => Visit t -> T t m (Maybe PcEnv)
getLoopPcEnv visit = do
    side <- askProblemSideWithAnalysis tag
    fast <- askHook #fast
    constRetAssumptions <- (. WithTag tag) <$> askHook #constRetAssumptions
    let loop = fromJust $ outermostLoopContaining side.analysis.loopData visitAddr
    let isConst var =
            let alwaysCheck = case var.ty of
                    ExprTypeHtd -> True
                    ExprTypeDom -> True
                    _ -> False
             in not (loopIsComplex loop)
                    && (alwaysCheck || fast)
                    && isSyntacticConstant side constRetAssumptions var loop visitAddr
    prevPcEnvOpt <- getPcEnvTagged $ visit & #restrs %~ M.insert visitAddr (numberVC 0)
    for prevPcEnvOpt $ \(PcEnv _ prevEnv) -> do
        memCalls <- addLoopMemCalls side.problem.nodes loop <$> scanMemCallsEnv prevEnv
        let nonConsts = filter (not . isConst) [ NameTy name ty | (name, Expr ty _) <- M.toList prevEnv ]
        env <- addVarReps
            VarRepRequestKindLoop
            (\ident -> printf "%P_after_loop_at_%P" ident visit.nodeId)
            memCalls
            visit
            nonConsts
            prevEnv
        pc <- liftFlat $ smtExprE boolT . NotSplit . nameS <$>
            addVar (printf "pc_of_loop_at_%P" visit.nodeId) boolT
        return $ PcEnv pc env
  where
    tag = visit.tag
    visitAddr = nodeAddrOf visit.nodeId

addLoopMemCalls :: NodeMap -> Loop -> MemCallsIfKnown -> MemCallsIfKnown
addLoopMemCalls nodes loop = fmap $ \memCalls ->
    let memberNodes = M.restrictKeys nodes loop.members
        fnames = S.fromList $ memberNodes ^.. folded % #_NodeCall % #functionName
     in foldl (flip addUnboundedCalls) memCalls (toList fnames)

getArcPcEnvs :: C t m => NodeAddr -> Visit t -> T t m [PcEnv]
getArcPcEnvs pred_ visit = do
    r <- runExceptT $ do
        prevs <- lift $ askPredVisits visit >>= pruneVisits . filter (\prev -> prev.nodeId == Addr pred_)
        ensureM $ length prevs <= 1
        fmap catMaybes $ for prevs $ \prev -> do
            checkGenerality prev
            lift $ getArcPcEnv prev visit
    case r of
        Right x -> return x
        Left (TooGeneral { split }) ->
            concat <$> traverse (getArcPcEnvs pred_) (splitVisitAt split visit)

getArcPcEnv :: C t m => Visit t -> Visit t -> T t m (Maybe PcEnv)
getArcPcEnv prev visit = runMaybeT $ do
    pcEnvs <- withMapSlotWithMapping lift #arcPcEnvs prev $ do
        MaybeT $ getPcEnvTagged prev
        lift $ emitNode prev
    hoistMaybe $ pcEnvs !? visit.nodeId

warmPcEnvCache :: C t m => Visit t -> T t m ()
warmPcEnvCache visit = go iters [] visit >>= traverse_ getPcEnvTagged
  where
    go 0 prevChain _ = return prevChain
    go i prevChain curVisit = do
        let f prev = do
                checkGenerality prev
                present <- lift $ liftPure $ use $ #nodePcEnvs % to (M.member prev)
                return $ not present && prev.restrs == curVisit.restrs
        runExceptT (lift (askPredVisits curVisit >>= pruneVisits) >>= filterM f) >>= \case
            Right (v:_) -> go (i - 1) (v:prevChain) v
            _ -> return prevChain
    iters = 5000 :: Integer

emitNode :: C t m => Visit t -> T t m (M.Map NodeId PcEnv)
emitNode visit = do
    pcEnv@(PcEnv pc env) <- fromJust <$> getPcEnvTagged visit
    node <- askNode visit
    M.fromList <$>
        if pc == falseE
        then return [ (cont, PcEnv falseE M.empty) | cont <- node ^.. nodeConts ]
        else case node of
            NodeCond condNode | condNode.left == condNode.right -> do
                return [(condNode.left, pcEnv)]
            NodeCond condNode | condNode.expr == trueE -> do
                return [(condNode.left, pcEnv), (condNode.right, PcEnv falseE env)]
            NodeBasic basicNode -> do
                updates <- for basicNode.varUpdates $ \update -> do
                    val <- case update.val.value of
                        ExprValueVar name -> return $ env ! name
                        _ -> do
                            let name = localName visit update.var.name
                            smtExprE update.var.ty <$> flattenAndAddDef env name update.val
                    return (update.var.name, val)
                return [(basicNode.next, PcEnv pc (M.union (M.fromList updates) env))]
            NodeCond condNode -> do
                let condNameHint = condName visit
                condIdent <- getFreshIdent condNameHint
                condDef <- flattenAndAddDef env condNameHint condNode.expr
                liftPure $ #condVars %= M.insert condDef condIdent
                let condEnv = M.singleton condIdent $ smtExprE boolT condDef
                let cond = flattenExpr condEnv (varE boolT condIdent)
                let lpc = andE cond pc
                let rpc = andE (notE cond) pc
                let env' = M.insert condIdent (smtExprE boolT condDef) env
                return [(condNode.left, PcEnv lpc env'), (condNode.right, PcEnv rpc env')]
            NodeCall callNode -> do
                liftPure $ #funCallOrder %= (Seq.|> visit)
                success <- liftFlat $ smtExprE boolT . NotSplit . nameS <$>
                    addVar (successName visit callNode.functionName) boolT
                ins <- liftFlat $ for callNode.input $ \arg -> smtExprE arg.ty <$> convertExpr' (flattenExpr env arg)
                memCalls <- fmap (addCall callNode.functionName) <$> scanMemCalls ins
                nonConstOutputs <- askNonConstOutputs visit.tag callNode
                env' <- addVarReps
                    VarRepRequestKindCall
                    (\name -> localName visit name)
                    memCalls
                    visit
                    nonConstOutputs
                    env
                let outs = [ env' ! out.name | out <- callNode.output ]
                let info = FunCallInfo { ins, outs, success }
                liftPure $ #funCalls %= M.insertWith undefined visit info
                let funName = applyTag visit callNode.functionName
                liftPure $ #funCallsByName %= M.insertWith (flip (<>)) funName [visit]
                AddFunAssertsHook addFunAsserts <- askHook #addFunAsserts
                addFunAsserts visit
                return [(callNode.next, PcEnv pc env')]

getFunCallInfo :: C t m => Visit t -> T t m FunCallInfo
getFunCallInfo unprunedVisit = do
    visit <- fromJust <$> pruneVisit unprunedVisit
    node <- askNode visit
    ensureM $ is #_NodeCall node
    opt <- liftPure $ use $ #funCalls % at visit
    whenNothing opt $ do
        askContVisit visit >>= getPcEnvTagged
        liftPure $ use $ #funCalls % expectingAt visit

instEqWithEnvs :: forall t m. C t m => (GraphExpr, ExprEnv) -> (GraphExpr, ExprEnv) -> T t m FlatExpr
instEqWithEnvs (x, xenv) (y, yenv) = do
    x' <- liftFlat $ convertUnderOp $ flattenExpr xenv x
    y' <- liftFlat $ convertUnderOp $ flattenExpr yenv y
    let f = case x'.ty of
            ExprTypeRelWrapper -> applyRelWrapper
            _ -> eqE
    return $ f x' y'
  where
    convertUnderOp :: C t m => FlatExpr -> GraphSliceSolverT m FlatExpr
    convertUnderOp expr = case expr.value of
        ExprValueOp op args -> do
            args' <- traverse convertInnerExpr args
            return $ Expr expr.ty $ ExprValueOp op args'
        _ -> convertInnerExpr expr
--

addFunAssertsHook :: t ~ AsmRefineTag => LookupFunctionSignature t -> Pairings t -> AddFunAssertsHook t
addFunAssertsHook lookupSig pairings = AddFunAssertsHook $ flip runReaderT env . addFunAssertsImpl
  where
    env = Env
        { lookupSig
        , pairings
        , pairingsAccess = M.fromList $ concatMap toList
            [ (,p) <$> withTags p | p <- M.keys pairings.unwrap]
        }

data AddFunAssertHookEnv t
  = Env
      { lookupSig :: LookupFunctionSignature t
      , pairings :: Pairings t
      , pairingsAccess :: M.Map (WithTag t Ident) (PairingId t)
      }
  deriving (Generic)

addFunAssertsImpl :: AsmRefineC t m => Visit t -> ReaderT (AddFunAssertHookEnv t) (T t m) ()
addFunAssertsImpl visit = do
    funName <- lift $ applyTag visit <$> askFunName visit
    pairingIdOpt <- gview $ #pairingsAccess % at funName
    for_ pairingIdOpt $ \pairingId -> do
        let otherFunName = viewAtTag (otherTag visit.tag) (withTags pairingId)
        group <- lift $ liftPure $ use $ #funCallsByName % to (M.findWithDefault [] otherFunName)
        for_ group $ \otherVisit -> do
            let visits = byTagFrom $ \tag' -> if tag' == visit.tag then visit else otherVisit
            compat <- areFunCallsCompatible visits
            when compat $ do
                imp <- getFunAssert visits
                lift $ liftFlat $ assertFact $ weakenAssert imp

areFunCallsCompatible :: AsmRefineC t m => ByTag t (Visit t) -> ReaderT (AddFunAssertHookEnv t) (T t m) Bool
areFunCallsCompatible visits = do
    lookupSig <- gview #lookupSig
    pairingsAccess <- gview #pairingsAccess
    lift $ do
        memCallsOpt <- for visits $ \v -> do
            info <- liftPure $ use $ #funCalls % expectingAt v
            scanMemCalls info.ins
        return $ areCallCountsCompatibleCompat' lookupSig (`M.lookup` pairingsAccess) memCallsOpt

getFunAssert :: RefineC t m => ByTag t (Visit t) -> ReaderT (AddFunAssertHookEnv t) (T t m) FlatExpr
getFunAssert visits = do
    pairingId <- lift $ for visits askFunName
    pairing <- gview $ #pairings % #unwrap % expectingAt pairingId
    lookupSig <- gview #lookupSig
    lift $ do
        let sigs = lookupSig <$> withTags pairingId
        lowLevelInfoByTag <- for visits $ \key ->
            liftPure $ use $ #funCalls % expectingAt key
        let info = augmentFunCallInfo <$> sigs <*> lowLevelInfoByTag
        pcs <- for (withTags visits) $ \visit -> getPcTagged visit.value
        let instEqs eqs = for eqs $ \eq ->
                instEqWithEnvs
                    (eq.lhs.expr, envForQuadrant eq.lhs.quadrant info)
                    (eq.rhs.expr, envForQuadrant eq.rhs.quadrant info)
        inEqs <- instEqs pairing.inEqs
        outEqs <- instEqs pairing.outEqs
        return $ impliesE
            (foldr1 andE (inEqs ++ [pcs.right]))
            (foldr1 andE (outEqs ++ [info.right.success `impliesE` info.left.success]))
  where
    envForQuadrant :: Tag t => PairingEqSideQuadrant t -> ByTag t FunCallInfoWithNames -> ExprEnv
    envForQuadrant (PairingEqSideQuadrant t direction) = view $ atTag t % directionLabel
      where
        directionLabel = case direction of
            PairingEqDirectionIn -> #ins
            PairingEqDirectionOut -> #outs

data FunCallInfoWithNames
  = FunCallInfoWithNames
      { ins :: ExprEnv
      , outs :: ExprEnv
      , success :: FlatExpr
      }
  deriving (Eq, Generic, Ord, Show)

augmentFunCallInfo :: FunctionSignature -> FunCallInfo -> FunCallInfoWithNames
augmentFunCallInfo sig info = FunCallInfoWithNames
    { ins = M.fromList (zip (map (.name) sig.input) info.ins)
    , outs = M.fromList (zip (map (.name) sig.output) info.outs)
    , success = info.success
    }

--

areCallCountsCompatibleCompat'
    :: t ~ AsmRefineTag
    => LookupFunctionSignature t
    -> (WithTag t Ident -> Maybe (PairingId t))
    -> ByTag t (Maybe CallCount)
    -> Bool
areCallCountsCompatibleCompat' lookupSig lookupPairingId callsOpt = case sequenceA callsOpt of
    Nothing -> True
    Just calls ->
        let lookupPairingIdOpt fun = do
                pid <- lookupPairingId fun
                guard $ any (\arg -> arg.ty == memT) (lookupSig (withTags pid).right).output
                return pid
         in areCallCountsCompatible lookupPairingIdOpt calls

asmRefineIsStackHook :: HasTagIsAsm t => ArgRenames t -> VarRepRequestKind -> WithTag t NameTy -> Maybe VarReqRequest
asmRefineIsStackHook argRenames kind var =
    if cond then Just req else Nothing
  where
    quadrant = PairingEqSideQuadrant var.tag PairingEqDirectionIn
    spName = argRenames quadrant (Ident "r13")
    cond = and
        [ tagIsAsm var.tag
        , var.value.ty == ExprTypeMem
        , "stack" `isPrefixOf` var.value.name.unwrap
        , kind /= VarRepRequestKindInit
        ]
    req = VarRepRequestSplitMem
        { addr = varE word32T spName
        }

--

getAllPcEnvs :: C t m => T t m [(Visit t, PcEnv)]
getAllPcEnvs = do
    inboundVisitInfo <- liftPure $ use #nodePcEnvs
    return
        [ (norm, pcEnv)
        | (norm, Just pcEnv) <- M.toList inboundVisitInfo
        ]
