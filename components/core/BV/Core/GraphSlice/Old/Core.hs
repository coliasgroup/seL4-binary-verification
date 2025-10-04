{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.GraphSlice.Old.Core
    ( FunCallInfo (..)
    , GraphSliceHooks (preEmitCallNodeHook)
    , GraphSliceT
    , TooGeneral (..)
    , VarRepRequestKind (..)
    , VarReqRequest (..)
    , askContVisit
    , askLoopData
    , askNodeGraph
    , askProblem
    , asmRefineGraphSliceHooks
    , defaultGraphSliceHooks
    , flattenExpr
    , getFunCallInfo
    , getInductVar
    , getNodePcEnv
    , getPc
    , instEqWithEnvs
    , runGraphSliceTStep
    , tryGetNodePcEnv
    ) where

import BV.Core.GraphSlice.Old.Solver

import BV.Core.GraphSlice.New (FlatExpr)
import BV.Core.GraphSlice.New.Common
import BV.Core.GraphSlice.New.Flatten.MemCalls
import BV.Core.GraphSlice.New.Flatten.NameHint
import BV.Core.GraphSlice.New.Flatten.PcEnv
import BV.Core.GraphSlice.New.Flatten.Tagged

import BV.Core.GenerateFreshName (generateFreshName)
import BV.Core.Logic
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils
import BV.Utils

import Control.Monad (filterM, guard, unless, when, (>=>))
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
import Data.List (isPrefixOf, sort)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))
import Text.Printf (printf)

type T = GraphSliceT

type TaggedT t m = GraphSliceTaggedT t (T t m)

type InnerT = GraphSliceSolverT

type C t m = (Tag t, MonadGraphSliceSendSExpr m)

type RefineC t m = (C t m, RefineTag t)

type AsmRefineC t m = (C t m, t ~ AsmRefineTag)

type TaggedC t n m = (C t n, MonadLiftUntaggedGeneric t n m)

--

newtype GraphSliceT t m a
  = GraphSliceT { run :: StateT (TState t) (ReaderT (TEnv t m) (InnerT m)) a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance MonadTrans (T t) where
    lift = liftInner . lift

instance MonadInner InnerT (T t) where
    liftInner = GraphSliceT . lift . lift

runGraphSliceTStep
    :: (Tag t, MonadGraphSliceSendSExpr m)
    => Problem t
    -> GraphSliceHooks t m
    -> T t m a
    -> InnerT m a
runGraphSliceTStep problem hooks m =
      flip runReaderT (initEnv problem hooks)
    . flip evalStateT initState
    . (.run)
    $ m'
  where
    m' = do
        initGraphSlice
        m

data TEnv t m
  = TEnv
      { problem :: Problem t
      , analysis :: ProblemAnalysis t
      , hooks :: GraphSliceHooks t m
      }
  deriving (Generic)

-- TODO abuse of PairingEqDirection
data GraphSliceHooks t n
  = GraphSliceHooks
      { preEmitCallNodeHook :: Visit -> TaggedT t n ()
      , postEmitCallNodeHook :: Visit -> TaggedT t n ()
      , isStackHook :: VarRepRequestKind -> WithTag t NameTy -> Maybe VarReqRequest
      }
  deriving (Generic)

data TState t
  = TState
      { inpEnvs :: Map NodeId ExprEnv
      , memCalls :: Map SmtName MemCalls
      , nodePcEnvs :: Map (WithTag t Visit) (Maybe PcEnv)
      , arcPcEnvs :: Map (WithTag t Visit) (Map NodeId PcEnv)
      , inductVarEnv :: Map EqHypInduct SmtName
      , condVars :: Map MaybeSplit Ident
      , contractions :: Map SExprWithPlaceholders FlatExpr
      , extraProblemNames :: S.Set Ident
      , hasInnerLoop :: Map (WithTag t NodeAddr) Bool
      , funCalls :: M.Map (WithTag t Visit) FunCallInfo
      , funCallsByName :: M.Map (WithTag t Ident) [Visit]
      }
  deriving (Generic)

data FunCallInfo
  = FunCallInfo
      { ins :: [FlatExpr]
      , outs :: [FlatExpr]
      , success :: FlatExpr
      }
  deriving (Eq, Generic, Ord, Show)

initEnv :: Tag t => Problem t -> GraphSliceHooks t m -> TEnv t m
initEnv problem hooks = TEnv
    { problem
    , analysis = analyzeProblem problem
    , hooks
    }

defaultGraphSliceHooks :: Monad m => GraphSliceHooks t m
defaultGraphSliceHooks = GraphSliceHooks
    { preEmitCallNodeHook = \_ -> return ()
    , postEmitCallNodeHook = \_ -> return ()
    , isStackHook = \_ _ -> Nothing
    }

asmRefineGraphSliceHooks
    :: AsmRefineC t m
    => LookupFunctionSignature t
    -> Pairings'
    -> ArgRenames t
    -> GraphSliceHooks t m
asmRefineGraphSliceHooks lookupSig pairings argRenames = defaultGraphSliceHooks
    & #postEmitCallNodeHook .~ addFunAssertsHook lookupSig pairings
    & #isStackHook .~ asmRefineIsStackHook argRenames

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
    , hasInnerLoop = M.empty
    , funCalls = M.empty
    , funCallsByName = M.empty
    }

--

class (Monad n, Monad m) => MonadLiftUntaggedGeneric t n m | m -> t, m -> n where
    liftUntaggedGeneric :: T t n a -> m a

instance Monad n => MonadLiftUntaggedGeneric t n (T t n) where
    liftUntaggedGeneric = id

instance Monad n => MonadLiftUntaggedGeneric t n (TaggedT t n) where
    liftUntaggedGeneric = liftUntagged

liftFlat :: MonadLiftUntaggedGeneric t n m => InnerT n a -> m a
liftFlat = liftUntaggedGeneric . liftInner

liftPure :: MonadLiftUntaggedGeneric t n m => StateT (TState t) (Reader (TEnv t n)) a -> m a
liftPure = liftUntaggedGeneric . GraphSliceT . mapStateT (mapReaderT (return . runIdentity))

--

initGraphSlice :: C t m => T t m ()
initGraphSlice = do
    addInputEnvs

--

withMapSlotWithMapping :: (TaggedC t n m, Monad m', Ord k) => (forall a. m a -> m' a) -> Lens' (TState t) (M.Map k v) -> k -> m' v -> m' v
withMapSlotWithMapping f = withMapSlotWith $ f . liftPure . mapStateT (return . runIdentity)

withMapSlot :: (TaggedC t n m, Ord k) => Lens' (TState t) (M.Map k v) -> k -> m v -> m v
withMapSlot = withMapSlotWithMapping id

withMapSlotTagged :: (C t m, Ord k) => Lens' (TState t) (M.Map (WithTag t k) v) -> k -> TaggedT t m v -> TaggedT t m v
withMapSlotTagged l k m = do
    k' <- askWithTag k
    withMapSlot l k' m

--

askHook :: TaggedC t n m => Lens' (GraphSliceHooks t n) a -> m a
askHook l = liftPure $ gview $ #hooks % l

askProblem :: TaggedC t n m => m (Problem t)
askProblem = liftPure $ gview #problem

askNode :: TaggedC t n m => NodeAddr -> m Node
askNode addr = liftPure $ gview $ #problem % #nodes % expectingAt addr

askNodeGraph :: TaggedC t n m => m NodeGraph
askNodeGraph = liftPure $ gview $ #analysis % #nodeGraph

askIsNonTriviallyReachableFrom :: TaggedC t n m => NodeAddr -> NodeId -> m Bool
askIsNonTriviallyReachableFrom from to_ = do
    g <- liftPure $ gview $ #analysis % #nodeGraph
    fromNode <- askNode from
    return $ or [ isReachableFrom g fromCont to_ | fromCont <- fromNode ^.. nodeConts ]

askLoopData :: C t m => TaggedT t m LoopData
askLoopData = liftPure $ gview $ #analysis % #loopData

askLoopHead :: C t m => NodeAddr -> TaggedT t m (Maybe NodeAddr)
askLoopHead n = loopHeadOf n <$> askLoopData

askLoopBody :: C t m => NodeAddr -> TaggedT t m (S.Set NodeAddr)
askLoopBody n = loopBodyOf n <$> askLoopData

askLoopContaining :: C t m => NodeAddr -> TaggedT t m Loop
askLoopContaining n = fromJust . flip loopContainingOf n <$> askLoopData

getHasInnerLoop :: C t m => NodeAddr -> TaggedT t m Bool
getHasInnerLoop loopHead = withMapSlotTagged #hasInnerLoop loopHead $ do
    p <- liftPure $ gview #problem
    loop <- askLoopContaining loopHead
    return $ not $ null $ innerLoopsOf p.nodes loop

askFunName :: C t m => Visit -> TaggedT t m Ident
askFunName v = view (expecting #_NodeCall % #functionName) <$> askNode (nodeAddrOf v.nodeId)

askPreds :: C t m => NodeId -> TaggedT t m (Set NodeAddr)
askPreds n = do
    tag <- askTag
    liftPure $ gview $ #analysis % #preds % atTag tag % to ($ n)

askPredVisits :: C t m => Visit -> TaggedT t m [Visit]
askPredVisits visit = do
    tag <- askTag
    preds <- liftPure $ gview $ #analysis % #preds % atTag tag
    return $ predVisits visit (toList (preds visit.nodeId))

askContVisits :: C t m => Visit -> TaggedT t m [Visit]
askContVisits visit = do
    let addr = nodeAddrOf visit.nodeId
    node <- askNode addr
    return $ contVisits visit (toListOf nodeConts node)

askContVisit :: C t m => Visit -> TaggedT t m Visit
askContVisit visit = do
    conts <- askContVisits visit
    let [cont] = conts
    return cont

--

getFreshIdent :: TaggedC t n m => NameHint -> m Ident
getFreshIdent nameHint = do
    problemNames <- liftPure $ gview $ #analysis % #varNames
    extraProblemNames <- liftPure $ use #extraProblemNames
    let taken n = S.member n problemNames || S.member n extraProblemNames
    let n = Ident $ generateFreshName (taken . Ident) nameHint
    liftPure $ #extraProblemNames %= S.insert n
    return n

maybeContract :: TaggedC t n m => Visit -> Ident -> FlatExpr -> m FlatExpr
maybeContract visit name expr@(Expr ty (ExprValueSMTExpr ms)) = case ms of
    NotSplit sexpr | length (showSExprWithPlaceholders sexpr) > 80 -> withMapSlot #contractions sexpr $ do
        let name' = localNameBefore visit name
        liftFlat $ smtExprE ty <$> addDef name' (smtExprE ty (NotSplit sexpr))
    _ -> return expr

contractPcEnv :: C t m => Visit -> PcEnv -> TaggedT t m PcEnv
contractPcEnv visit (PcEnv pc env) = do
    pc' <- case pc.value of
        ExprValueSMTExpr _ -> return pc
        _ -> do
            hint <- pathCondName <$> askWithTag visit
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

flattenAndAddDef :: TaggedC t n m => ExprEnv -> NameHint -> GraphExpr -> m MaybeSplit
flattenAndAddDef env nameHint val = liftFlat $ addDef nameHint $ flattenExpr env val

--

getMemCalls :: TaggedC t n m => SExprWithPlaceholders -> m MemCalls
getMemCalls = liftFlat . getImmBasisMems >=> \mems -> fmap (foldr1 mergeMemCalls) $ for (S.toList mems) $ \mem ->
    liftPure $ use $ #memCalls % expectingAt mem

scanMemCallsEnv :: TaggedC t n m => ExprEnv -> m MemCallsIfKnown
scanMemCallsEnv = scanMemCalls . toList

scanMemCalls :: TaggedC t n m => [FlatExpr] -> m MemCallsIfKnown
scanMemCalls tyVals = do
    memCalls <- traverse getMemCalls [ v | Expr ty (ExprValueSMTExpr (NotSplit v)) <- tyVals, ty == memT ]
    return $ case memCalls of
        [] -> Nothing
        _ -> Just $ foldr1 mergeMemCalls memCalls

--

pruneVisit :: C t m => Visit -> TaggedT t m (Maybe Visit)
pruneVisit visit = runMaybeT $
    forOf #restrs visit $ \restrs ->
        fmap (sort . concat) $ for restrs $ \restr -> do
            reachable <- lift $ askIsNonTriviallyReachableFrom restr.nodeAddr visit.nodeId
            guard $ reachable || hasZeroVC restr.visitCount
            return $ [ restr | reachable ]

pruneVisits :: C t m => [Visit] -> TaggedT t m [Visit]
pruneVisits visits = catMaybes <$> traverse pruneVisit visits

data TooGeneral
  = TooGeneral
      { split :: NodeAddr
      }
  deriving (Eq, Generic, Ord, Show)

checkGenerality :: C t m => Visit -> ExceptT TooGeneral (TaggedT t m) ()
checkGenerality visit = void $ runMaybeT $ do
    nodeAddr <- hoistMaybe $ preview #_Addr visit.nodeId
    loopId <- MaybeT $ lift $ askLoopHead nodeAddr
    for_ visit.restrs $ \restr -> do
        loopIdOpt' <- lift $ lift $ askLoopHead restr.nodeAddr
        when (loopIdOpt' == Just loopId && isOptionsVC restr.visitCount) $ do
            throwError $ TooGeneral { split = restr.nodeAddr }

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

type MemCallsIfKnown = Maybe MemCalls

-- TODO rename?
addVarReps
    :: C t m
    => VarRepRequestKind
    -> (Ident -> NameHint)
    -> MemCallsIfKnown
    -> Visit
    -> [NameTy]
    -> ExprEnv
    -> TaggedT t m ExprEnv
addVarReps kind mkName memCalls visit vars = execStateT $ do
    for_ vars $ \var -> do
        v <- lift $ smtExprE var.ty . NotSplit . nameS <$> addVarWithMemCalls (mkName var.name) var.ty memCalls
        modify $ M.insert var.name v
    intermediateEnv <- get
    for_ vars $ \var -> do
        opt <- lift $ varRepRequest kind visit intermediateEnv var
        for_ opt $ \splitMem -> modify $ M.insert var.name $ smtExprE var.ty $ Split splitMem

addVarWithMemCalls :: TaggedC t n m => NameHint -> ExprType -> MemCallsIfKnown -> m SmtName
addVarWithMemCalls nameHint ty memCallsOpt = do
    v <- liftFlat $ addVar nameHint ty
    when (isMemT ty) $ do
        liftPure $ #memCalls %= M.insert v (fromJust memCallsOpt)
    return v

varRepRequest :: C t m => VarRepRequestKind -> Visit -> ExprEnv -> NameTy -> TaggedT t m (Maybe SplitMem)
varRepRequest kind visit env var = do
    isStackHook <- askHook #isStackHook
    reqOpt <- askWithTag var <&> isStackHook kind
    for reqOpt $ \req -> case req of
        VarRepRequestSplitMem { addr } -> do
            addrSExpr <- liftFlat $ convertExprNotSplit $ flattenExpr env addr
            let nameHint = printf "%P_for_%s" var.name (nodeCountName visit)
            liftFlat $ addSplitMemVar addrSExpr nameHint var.ty

--

-- HACK
updatePcEnvCompat :: TaggedC t n m => PcEnv -> m PcEnv
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

getInductVar :: TaggedC t n m => EqHypInduct -> m FlatExpr
getInductVar induct =
    fmap (smtExprE ty . NotSplit . nameS) $
        withMapSlot #inductVarEnv induct $
            liftFlat $ addVar (printf "induct_i_%d_%d" induct.n1 induct.n2) ty
  where
    ty = word32T

getPc :: C t m => Visit -> TaggedT t m FlatExpr
getPc visit = getNodePcEnv visit >>= \case
    Nothing -> return falseE
    Just (PcEnv pc _) -> liftFlat $ convertInnerExpr pc

getNodePcEnv :: C t m => Visit -> TaggedT t m (Maybe PcEnv)
getNodePcEnv = runIdentityT . getNodePcEnvInner (const (return ()))

tryGetNodePcEnv :: C t m => Visit -> TaggedT t m (Either TooGeneral (Maybe PcEnv))
tryGetNodePcEnv visit = runExceptT (getNodePcEnvInner checkGenerality visit)

getNodePcEnvInner :: (C t m, MonadTrans trans) => (Visit -> trans (TaggedT t m) ()) -> Visit -> trans (TaggedT t m) (Maybe PcEnv)
getNodePcEnvInner check unprunedVisit = runMaybeT $ do
    visit <- MaybeT $ lift $ pruneVisit unprunedVisit
    lift $ check visit
    MaybeT $ lift $ withMapSlotTagged #nodePcEnvs visit $ do
        warmPcEnvCache visit
        getNodePcEnvRaw visit

getNodePcEnvRaw :: C t m => Visit -> TaggedT t m (Maybe PcEnv)
getNodePcEnvRaw visit = do
    liftPure (use $ #inpEnvs % at visit.nodeId) >>= \case
        Just env -> return $ Just $ PcEnv trueE env
        Nothing -> do
            let f restr = Addr restr.nodeAddr == visit.nodeId && restr.visitCount == offsetVC 0
            if any f visit.restrs
                then getLoopPcEnv visit
                else do
                    arcPcEnvs <- toListOf (folded % folded) <$> do
                        preds <- toList <$> askPreds visit.nodeId
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
    f (WithTag tag side) = runTagged tag $ do
        env <- addVarReps
            VarRepRequestKindInit
            (\name -> name.unwrap ++ "_init")
            (Just M.empty)
            (Visit side.entryPoint [])
            side.input
            M.empty
        liftPure $ #inpEnvs %= M.insert side.entryPoint env


getLoopPcEnv :: C t m => Visit -> TaggedT t m (Maybe PcEnv)
getLoopPcEnv visit = do
    prevPcEnvOpt <- getNodePcEnv $ visit & #restrs %~ withMapVC (M.insert visitAddr (numberVC 0))
    for prevPcEnvOpt $ \(PcEnv _ prevEnv) -> do
        memCalls <- scanMemCallsEnv prevEnv >>= addLoopMemCalls visitAddr
        nonConsts <- filterM (fmap not . isConstM) [ NameTy name ty | (name, Expr ty _) <- M.toList prevEnv ]
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
    visitAddr = nodeAddrOf visit.nodeId
    isConstM var = do
        let checkConst = case var.ty of
                ExprTypeHtd -> True
                ExprTypeDom -> True
                _ -> False
        if checkConst then isSyntacticConstant var visitAddr else return False

addLoopMemCalls :: C t m => NodeAddr -> MemCallsIfKnown -> TaggedT t m MemCallsIfKnown
addLoopMemCalls split = traverse $ \memCalls -> do
    nodeAddrs <- askLoopBody split
    node <- traverse askNode (toList nodeAddrs)
    let fnames = S.fromList $ node ^.. folded % #_NodeCall % #functionName
    return $ foldl (flip addUnboundedMemCalls) memCalls (toList fnames)

getArcPcEnvs :: C t m => NodeAddr -> Visit -> TaggedT t m [PcEnv]
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

getArcPcEnv :: C t m => Visit -> Visit -> TaggedT t m (Maybe PcEnv)
getArcPcEnv prev visit = runMaybeT $ do
    key <- lift $ askWithTag prev
    pcEnvs <- withMapSlotWithMapping lift #arcPcEnvs key $ do
        MaybeT $ getNodePcEnv prev
        lift $ emitNode prev
    hoistMaybe $ pcEnvs !? visit.nodeId

warmPcEnvCache :: C t m => Visit -> TaggedT t m ()
warmPcEnvCache visit = go iters [] visit >>= traverse_ getNodePcEnv
  where
    go 0 prevChain _ = return prevChain
    go i prevChain curVisit = do
        let f prev = do
                checkGenerality prev
                key <- lift $ askWithTag prev
                present <- lift $ liftPure $ use $ #nodePcEnvs % to (M.member key)
                return $ not present && prev.restrs == curVisit.restrs
        runExceptT (lift (askPredVisits curVisit >>= pruneVisits) >>= filterM f) >>= \case
            Right (v:_) -> go (i - 1) (v:prevChain) v
            _ -> return prevChain
    iters = 5000 :: Integer

emitNode :: C t m => Visit -> TaggedT t m (M.Map NodeId PcEnv)
emitNode visit = do
    pcEnv@(PcEnv pc env) <- fromJust <$> getNodePcEnv visit
    let nodeAddr = nodeAddrOf visit.nodeId
    node <- askNode nodeAddr
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
                preHook <- askHook #preEmitCallNodeHook
                preHook visit
                success <- liftFlat $ smtExprE boolT . NotSplit . nameS <$>
                    addVar (successName visit callNode.functionName) boolT
                ins <- liftFlat $ for callNode.input $ \arg -> smtExprE arg.ty <$> convertExpr' (flattenExpr env arg)
                memCalls <- fmap (addMemCall callNode.functionName) <$> scanMemCalls ins
                env' <- addVarReps
                    VarRepRequestKindCall
                    (\name -> localName visit name)
                    memCalls
                    visit
                    callNode.output
                    env
                let outs = [ env' ! out.name | out <- callNode.output ]
                key <- askWithTag visit
                let info = FunCallInfo { ins, outs, success }
                liftPure $ #funCalls %= M.insertWith undefined key info
                funName <- askWithTag callNode.functionName
                liftPure $ #funCallsByName %= M.insertWith (flip (<>)) funName [visit]
                postHook <- askHook #postEmitCallNodeHook
                postHook visit
                return [(callNode.next, PcEnv pc env')]

isSyntacticConstant :: C t m => NameTy -> NodeAddr -> TaggedT t m Bool
isSyntacticConstant var split = do
    hasInnerLoop <- getHasInnerLoop split
    if hasInnerLoop
        then return False
        else do
            loopSet <- askLoopBody split
            let go (name, addr) = do
                    node <- lift $ lift $ askNode addr
                    predName <- fromMaybe name <$> case node of
                        NodeCall callNode ->
                            if NameTy name var.ty `elem` callNode.output
                            then throwNotConst
                            else return Nothing
                        NodeBasic basicNode -> do
                            let updateExprs =
                                    [ u.val
                                    | u <- basicNode.varUpdates
                                    , u.var == NameTy name var.ty
                                    ]
                            case updateExprs of
                                [] -> return Nothing
                                [Expr _ (ExprValueVar ident)] -> return $ Just ident
                                [_] -> throwNotConst
                        _ -> return Nothing
                    preds <- lift $ lift $ S.intersection loopSet <$> askPreds (Addr addr)
                    for_ preds $ \predAddr -> do
                        let predVar = (predName, predAddr)
                        safe <- get
                        unless (predVar `S.member` safe) $ do
                            when (predAddr == split) throwNotConst
                            go predVar
                            modify $ S.insert predVar
            isRight <$>
                runExceptT
                    (evalStateT
                        (go (var.name, split))
                        (S.singleton (var.name, split)))
  where
    throwNotConst = throwError ()

getFunCallInfo :: (C t m, MonadError TooGeneral m) => Visit -> TaggedT t m FunCallInfo
getFunCallInfo unprunedVisit = do
    visit <- fromJust <$> pruneVisit unprunedVisit
    node <- askNode $ nodeAddrOf visit.nodeId
    ensureM $ is #_NodeCall node
    key <- askWithTag visit
    opt <- liftPure $ use $ #funCalls % at key
    whenNothing opt $ do
        askContVisit visit >>= getNodePcEnv
        liftPure $ use $ #funCalls % expectingAt key

instEqWithEnvs :: forall t n m. TaggedC t n m => (GraphExpr, ExprEnv) -> (GraphExpr, ExprEnv) -> m FlatExpr
instEqWithEnvs (x, xenv) (y, yenv) = do
    x' <- liftFlat $ convertUnderOp $ flattenExpr xenv x
    y' <- liftFlat $ convertUnderOp $ flattenExpr yenv y
    let f = case x'.ty of
            ExprTypeRelWrapper -> applyRelWrapper
            _ -> eqE
    return $ f x' y'
  where
    convertUnderOp :: C t n => FlatExpr -> GraphSliceSolverT n FlatExpr
    convertUnderOp expr = case expr.value of
        ExprValueOp op args -> do
            args' <- traverse convertInnerExpr args
            return $ Expr expr.ty $ ExprValueOp op args'
        _ -> convertInnerExpr expr
--

addFunAssertsHook :: AsmRefineC t m => LookupFunctionSignature t -> Pairings' -> Visit -> TaggedT t m ()
addFunAssertsHook lookupSig pairings = flip runReaderT env . addFunAsserts
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

addFunAsserts :: AsmRefineC t m => Visit -> ReaderT (AddFunAssertHookEnv t) (TaggedT t m) ()
addFunAsserts visit = do
    tag <- lift askTag
    funName <- lift $ askWithTag =<< askFunName visit
    pairingIdOpt <- gview $ #pairingsAccess % at funName
    for_ pairingIdOpt $ \pairingId -> do
        let otherFunName = viewAtTag (otherTag tag) (withTags pairingId)
        group <- lift $ liftUntagged $ liftPure $ use $ #funCallsByName % to (M.findWithDefault [] otherFunName)
        for_ group $ \otherVisit -> do
            let visits = byTagFrom $ \tag' -> if tag' == tag then visit else otherVisit
            compat <- mapReaderT liftUntagged $ areFunCallsCompatible visits
            when compat $ do
                imp <- mapReaderT liftUntagged $ getFunAssert visits
                lift $ liftFlat $ assertFact $ weakenAssert imp

areFunCallsCompatible :: AsmRefineC t m => ByTag t Visit -> ReaderT (AddFunAssertHookEnv t) (T t m) Bool
areFunCallsCompatible visits = do
    lookupSig <- gview #lookupSig
    pairingsAccess <- gview #pairingsAccess
    lift $ do
        memCallsOpt <- for (withTags visits) $ \vt -> do
            info <- liftPure $ use $ #funCalls % expectingAt vt
            scanMemCalls info.ins
        return $ areMemCallsCompatible lookupSig (pairingsAccess !) memCallsOpt

getFunAssert :: RefineC t m => ByTag t Visit -> ReaderT (AddFunAssertHookEnv t) (T t m) FlatExpr
getFunAssert visits = do
    pairingId <- lift $ forTagged visits askFunName
    pairing <- gview $ #pairings % #unwrap % expectingAt pairingId
    lookupSig <- gview #lookupSig
    lift $ do
        let sigs = lookupSig <$> withTags pairingId
        lowLevelInfoByTag <- for (withTags visits) $ \key ->
            liftPure $ use $ #funCalls % expectingAt key
        let info = augmentFunCallInfo <$> sigs <*> lowLevelInfoByTag
        pcs <- for (withTags visits) $ \visit -> runTagged visit.tag $ getPc visit.value
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

areMemCallsCompatible
    :: t ~ AsmRefineTag
    => LookupFunctionSignature t
    -> (WithTag t Ident -> PairingId t)
    -> ByTag t (Maybe MemCalls)
    -> Bool
areMemCallsCompatible lookupSig lookupPairingId callsOpt = case sequenceA callsOpt of
    Nothing -> True
    Just calls ->
        let hasMem cFunName =
                let cSig = lookupSig $ WithTag rightTag cFunName
                 in any (\arg -> arg.ty == memT) cSig.output
            cCastCalls = M.fromList $ catMaybes
                [ let pairingId = lookupPairingId (WithTag leftTag asmFunName)
                      cFunName = pairingId.right
                   in if hasMem cFunName
                      then Just (cFunName, asmCallsForFun)
                      else Nothing
                | (asmFunName, asmCallsForFun) <- M.toList calls.left
                ]
            compat cFunName =
                let cCast = M.findWithDefault zeroMemCallsRange cFunName cCastCalls
                    cActual = M.findWithDefault zeroMemCallsRange cFunName calls.c
                in memCallsRangesOverlap cCast cActual
         in all compat $ S.toList $ M.keysSet calls.right <> M.keysSet cCastCalls

asmRefineIsStackHook :: t ~ AsmRefineTag => ArgRenames t -> VarRepRequestKind -> WithTag t NameTy -> Maybe VarReqRequest
asmRefineIsStackHook argRenames kind var =
    if cond then Just req else Nothing
  where
    quadrant = PairingEqSideQuadrant Asm PairingEqDirectionIn
    spName = argRenames quadrant (Ident "r13")
    cond = and
        [ var.tag == Asm
        , var.value.ty == ExprTypeMem
        , "stack" `isPrefixOf` var.value.name.unwrap
        , kind /= VarRepRequestKindInit
        ]
    req = VarRepRequestSplitMem
        { addr = varE word32T spName
        }
