{-# LANGUAGE MultiWayIf #-}

-- TODO
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.Core.RepGraph.New.FlattenGraph
    ( ExprEnv
    , FunCallInfo (..)
    , PcEnv (..)
    , RepGraphHooks (preEmitCallNodeHook)
    , RepGraphT
    , RepGraphTaggedT
    , addPValidDomAssertions
    , askCont
    , askLoopData
    , askNodeGraph
    , askProblem
    , askTag
    , askWithTag
    , asmRefineRepGraphHooks
    , defaultRepGraphHooks
    , getFunCallInfo
    , getInductVar
    , getNodePcEnv
    , getNodePcEnvWithTag
    , getPc
    , getPcWithTag
    , instEqWithEnvs
    , runRepGraphTStep
    , runTagged
    , tryGetNodePcEnv
    , withEnv
    ) where

import BV.Core.RepGraph.New.Common
import BV.Core.RepGraph.New.Flat
import BV.Core.RepGraph.New.FlattenGraph.MemCalls
import BV.Core.RepGraph.New.FlattenGraph.NameHint
import BV.Core.RepGraph.New.FlattenGraph.PcEnv
import BV.Core.RepGraph.New.SendFlatExprCommand (FlatExpr)
import BV.Core.RepGraph.New.SendSolverExprCommand (MonadRepGraphSendSExpr)

import BV.Core.Logic (applyRelWrapper, weakenAssert)
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils (whenNothing, withMapSlotWith)
import BV.Utils

import Control.Monad (filterM, guard, unless, when, (>=>))
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (runIdentity, runIdentityT)
import Control.Monad.Reader (Reader, ReaderT, ask, asks, mapReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, mapStateT, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), hoistMaybe, runMaybeT)
import Data.Either (isRight)
import Data.Foldable (for_, toList, traverse_)
import Data.Functor (void)
import Data.List (sort)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))
import Text.Printf (printf)

-- TODO
cheatMemDoms :: Bool
cheatMemDoms = True

--

type T = RepGraphT

type TaggedT = RepGraphTaggedT

type InnerT = RepGraphFlatT

type C t m = (Tag t, MonadRepGraphSendSExpr m)

type RefineC t m = (C t m, RefineTag t)

type AsmRefineC t m = (C t m, t ~ AsmRefineTag)

type TaggedC t n m = (C t n, MonadInner (InnerT n) m, MonadT t n m)

--

newtype RepGraphT t m a
  = RepGraphT { run :: StateT (TState t) (ReaderT (TEnv t m) (InnerT m)) a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance Monad m => MonadInner (InnerT m) (T t m) where
    liftInner = RepGraphT . lift . lift

instance MonadTrans (T t) where
    lift = liftInner . lift

runRepGraphTStep
    :: (Tag t, Monad m)
    => Problem t
    -> RepGraphHooks t m
    -> T t m a
    -> InnerT m a
runRepGraphTStep problem hooks =
      flip runReaderT (initEnv problem hooks)
    . flip evalStateT initState
    . (.run)

data TEnv t m
  = TEnv
      { problem :: Problem t
      , analysis :: ProblemAnalysis t
      , hooks :: RepGraphHooks t m
      }
  deriving (Generic)

data RepGraphHooks t n
  = RepGraphHooks
      { preEmitCallNodeHook :: Visit -> TaggedT t n ()
      , postEmitCallNodeHook :: Visit -> TaggedT t n ()
      , isStackHook :: WithTag t Ident -> PairingEqDirection -> NameTy -> Bool
      , stackPointerHook :: t -> Maybe GraphExpr
      }
  deriving (Generic)

data TState t
  = TState
      { inpEnvs :: Map (WithTag t ()) ExprEnv
      , nodePcEnvs :: Map (WithTag t Visit) (Maybe PcEnv)
      , arcPcEnvs :: Map (WithTag t Visit) (Map NodeId PcEnv)
      , inductVarEnv :: Map EqHypInduct NameTy
      , contractions :: Map FlatExpr Ident
      , hasInnerLoop :: Map (WithTag t NodeAddr) Bool
      , stacks :: Set Ident
      , memCalls :: Map Ident MemCalls
      , funCalls :: M.Map (WithTag t Visit) FunCallInfo
      , funCallsByName :: M.Map (WithTag t Ident) [Visit]
      }
  deriving (Generic)

initEnv :: Tag t => Problem t -> RepGraphHooks t m -> TEnv t m
initEnv problem hooks = TEnv
    { problem
    , analysis = analyzeProblem problem
    , hooks
    }

defaultRepGraphHooks :: Monad m => RepGraphHooks t m
defaultRepGraphHooks = RepGraphHooks
    { preEmitCallNodeHook = \_ -> return ()
    , postEmitCallNodeHook = \_ -> return ()
    , isStackHook = \_ _ _ -> False
    , stackPointerHook = \_ -> Nothing
    }

asmRefineRepGraphHooks
    :: AsmRefineC t m
    => LookupFunctionSignature t
    -> Pairings'
    -> ArgRenames t
    -> RepGraphHooks t m
asmRefineRepGraphHooks lookupSig pairings argRenames = defaultRepGraphHooks
    & #postEmitCallNodeHook .~ addFunAssertsHook lookupSig pairings
    & #isStackHook .~ asmRefineIsStackHook
    & #stackPointerHook .~ asmRefineStackPointerHook argRenames

initState :: TState t
initState = TState
    { inpEnvs = M.empty
    , nodePcEnvs = M.empty
    , arcPcEnvs = M.empty
    , inductVarEnv = M.empty
    , contractions = M.empty
    , hasInnerLoop = M.empty
    , stacks = S.empty
    , memCalls = M.empty
    , funCalls = M.empty
    , funCallsByName = M.empty
    }

--

newtype RepGraphTaggedT t m a
  = RepGraphTaggedT { run :: ReaderT t (T t m) a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance Monad m => MonadInner (InnerT m) (TaggedT t m) where
    liftInner = RepGraphTaggedT . lift . liftInner

instance MonadTrans (TaggedT t) where
    lift = liftInner . lift

askTag :: Monad m => TaggedT t m t
askTag = RepGraphTaggedT ask

askWithTag :: Monad m => a -> TaggedT t m (WithTag t a)
askWithTag a = do
    tag <- askTag
    return $ WithTag tag a

runTagged :: Monad m => t -> TaggedT t m a -> T t m a
runTagged tag m = runReaderT m.run tag

liftUntagged :: Monad m => T t m a -> TaggedT t m a
liftUntagged = RepGraphTaggedT . lift

class (Monad n, Monad m) => MonadT t n m | m -> t, m -> n where
    liftPure :: StateT (TState t) (Reader (TEnv t n)) a -> m a

instance Monad n => MonadT t n (T t n) where
    liftPure = RepGraphT . mapStateT (mapReaderT (return . runIdentity))

instance (Monad n, MonadT t n (T t n)) => MonadT t n (TaggedT t n) where
    liftPure = liftUntagged . liftPure

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

askHook :: TaggedC t n m => Lens' (RepGraphHooks t n) a -> m a
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

askFunName :: (Tag t, MonadRepGraphSendSExpr n) => Visit -> T t n Ident
askFunName v = do
    p <- askProblem
    return $ p ^. #nodes % at (nodeAddrOf v.nodeId) % unwrapped % expecting #_NodeCall % #functionName

askPreds :: C t m => NodeId -> TaggedT t m (Set NodeAddr)
askPreds n = do
    tag <- askTag
    liftPure $ gview $ #analysis % #preds % atTag tag % to ($ n)

askPrevs :: C t m => Visit -> TaggedT t m [Visit]
askPrevs visit = do
    preds <- toList <$> askPreds visit.nodeId
    let f pred_ = Visit (Addr pred_) <$> incrVCs visit.restrs pred_ (-1)
    return $ mapMaybe f preds

askCont :: TaggedC t n m => Visit -> m Visit
askCont visit = do
    let addr = nodeAddrOf visit.nodeId
    conts <- toListOf nodeConts <$> askNode addr
    let [cont] = conts
    return $ Visit
        { nodeId = cont
        , restrs = fromJust $ incrVCs visit.restrs addr 1
        }

incrVCs :: [Restr] -> NodeAddr -> Integer -> Maybe [Restr]
incrVCs restrs n incr = if
    | n `M.notMember` restrsMap -> Just restrs
    | isEmptyVC vcNew -> Nothing
    | otherwise -> Just (fromMapVC (M.insert n vcNew restrsMap))
  where
    restrsMap = toMapVC restrs
    vcOld = restrsMap ! n
    vcNew = incrVC incr vcOld

specialize :: Visit -> NodeAddr -> [Visit]
specialize visit split = ensure (isOptionsVC splitVC)
    [ visit & #restrs .~ fromMapVC (M.insert split (fromSimpleVC simpleVC) restrsMap)
    | simpleVC <- enumerateSimpleVCs splitVC
    ]
  where
    restrsMap = toMapVC visit.restrs
    splitVC = restrsMap ! split

contractPcEnv :: C t m => Visit -> PcEnv -> TaggedT t m PcEnv
contractPcEnv visit (PcEnv pc env) = do
    pc' <- do
        hint <- pathCondName <$> askWithTag visit
        liftInner $ varFromNameTyE <$> cacheExpr hint pc
    env' <- M.traverseWithKey f env
    return $ PcEnv pc' env'
  where
    f name val = liftInner $ varFromNameTyE <$> cacheExprInline (localNameBefore visit name) val

--

flattenExpr :: TaggedC t n m => GraphExpr -> ReaderT ExprEnv m FlatExpr
flattenExpr = traverseOf (exprArgs % traversed) flattenExpr >=> \expr -> case expr.value of
    ExprValueVar name -> do
        let err = error $ "env miss: " ++ show name
        asks $ M.findWithDefault err name
    _ -> return expr

data VarRepContext
  = VarRepContextInit
  | VarRepContextLoop
  | VarRepContextCall
      { funName :: Ident
      }
  deriving (Eq, Generic, Ord, Show)

-- TODO rename?
addVarReps
    :: C t m
    => VarRepContext
    -> (Ident -> NameHint)
    -> Maybe MemCalls
    -> [NameTy]
    -> ExprEnv
    -> TaggedT t m ExprEnv
addVarReps ctx mkName memCallsOpt vars env = do
    tag <- askTag
    isStackHook <- askHook #isStackHook
    newVars <- for vars $ \var -> (var.name,) <$> do
        isStack <-
            if not (isMemT var.ty)
                then return False
                else case ctx of
                    VarRepContextInit -> do
                        funName <- view (#sides % atTag tag % #name) <$> askProblem
                        return $ isStackHook (WithTag tag funName) PairingEqDirectionIn var
                    VarRepContextLoop -> do
                        isExprStack $ env ! var.name
                    VarRepContextCall { funName } -> do
                        return $ isStackHook (WithTag tag funName) PairingEqDirectionOut var
        let nameHint = mkName var.name
        if isStack && isn't #_VarRepContextInit ctx
            then do
                top <- liftInner $ addVar (nameHint ++ "_top") memT
                bottom <- liftInner $ addVar (nameHint ++ "_bot") memT
                addStack top.name
                addStack bottom.name
                stackPointerHook <- askHook #stackPointerHook
                let Just graphStackPointer = stackPointerHook tag
                flatStackPointer <- withEnv env $ flattenExpr graphStackPointer
                return $ splitMemE flatStackPointer
                    (varFromNameTyE top)
                    (varFromNameTyE bottom)
            else do
                newVar <- liftInner $ addVar nameHint var.ty
                when (isMemT var.ty) $ do
                    liftPure $ #memCalls %= M.insert newVar.name (fromJust memCallsOpt)
                when isStack $ addStack newVar.name
                return $ varFromNameTyE newVar
    return $ M.fromList newVars <> env
  where
    addStack name = liftPure $ #stacks %= S.insert name
    isExprStack = go
      where
        go expr = case expr.value of
            ExprValueOp OpMemUpdate [m, _, _] -> go m
            ExprValueOp OpIfThenElse [_, l, r] -> f <$> go l <*> go r
            ExprValueOp (OpExt OpExtSplitMem) [_, top, bottom] -> f <$> go top <*> go bottom
            ExprValueVar name -> liftInner (lookupDef name) >>= \case
                Just def -> go def
                Nothing -> liftPure $ use $ #stacks % to (name `S.member`)
        f x y = ensure (x == y) x

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

getInductVar :: TaggedC t n m => EqHypInduct -> m NameTy
getInductVar induct =
    withMapSlot #inductVarEnv induct $
        liftInner $
            addVar (inductVarName induct) word32T

getPcWithTag :: C t m => WithTag t Visit -> T t m FlatExpr
getPcWithTag (WithTag tag visit) = runTagged tag $ getPc visit

getPc :: C t m => Visit -> TaggedT t m FlatExpr
getPc visit = getNodePcEnv visit <&> \case
    Nothing -> falseE
    Just (PcEnv pc _) -> pc

getNodePcEnvWithTag :: C t m => WithTag t Visit -> T t m (Maybe PcEnv)
getNodePcEnvWithTag (WithTag tag visit) = runTagged tag $ getNodePcEnv visit

getNodePcEnv :: C t m => Visit -> TaggedT t m (Maybe PcEnv)
getNodePcEnv = runIdentityT . getNodePcEnvInner (const (return ()))

tryGetNodePcEnv :: C t m => Visit -> TaggedT t m (Either TooGeneral (Maybe PcEnv))
tryGetNodePcEnv = runExceptT . getNodePcEnvInner checkGenerality

getNodePcEnvInner :: (C t m, MonadTrans trans) => (Visit -> trans (TaggedT t m) ()) -> Visit -> trans (TaggedT t m) (Maybe PcEnv)
getNodePcEnvInner check unprunedVisit = runMaybeT $ do
    visit <- MaybeT $ lift $ pruneVisit unprunedVisit
    lift $ check visit
    MaybeT $ lift $ withMapSlotTagged #nodePcEnvs visit $ do
        warmPcEnvCache visit
        getNodePcEnvRaw visit

getNodePcEnvRaw :: C t m => Visit -> TaggedT t m (Maybe PcEnv)
getNodePcEnvRaw visit = do
    tag <- askTag
    side <- view (#sides % atTag tag) <$> askProblem
    if visit.nodeId == side.entryPoint
        then do
            env <- withMapSlotTagged #inpEnvs () $ addVarReps
                VarRepContextInit
                (\name -> name.unwrap ++ "_init")
                (Just M.empty)
                side.input
                M.empty
            return $ Just $ PcEnv trueE env
        else do
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
                                    Err -> traversed % #env .~ M.empty
                                    _ -> id
                            let pcEnv = mergePcEnvs (optimize arcPcEnvs)
                            contractPcEnv visit pcEnv

getLoopPcEnv :: C t m => Visit -> TaggedT t m (Maybe PcEnv)
getLoopPcEnv visit = do
    prevPcEnvOpt <- getNodePcEnv $ visit & #restrs %~ withMapVC (M.insert visitAddr (numberVC 0))
    for prevPcEnvOpt $ \(PcEnv _ prevEnv) -> do
        memCalls <- scanMemCallsHere (toList prevEnv) >>= addLoopMemCalls visitAddr
        nonConsts <- filterM (fmap not . isConstM) (toList (exprEnvVars prevEnv))
        env <- addVarReps
            VarRepContextLoop
            (\ident -> printf "%P_after_loop_at_%P" ident visit.nodeId)
            memCalls
            nonConsts
            prevEnv
        pc <- liftInner $ varFromNameTyE <$>
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

addLoopMemCalls :: C t m => NodeAddr -> Maybe MemCalls -> TaggedT t m (Maybe MemCalls)
addLoopMemCalls split = traverse $ \memCalls -> do
    nodeAddrs <- askLoopBody split
    nodes <- traverse askNode (toList nodeAddrs)
    let fnames = S.fromList $ nodes ^.. folded % #_NodeCall % #functionName
    return $ foldl (flip addUnboundedMemCalls) memCalls (toList fnames)

getArcPcEnvs :: C t m => NodeAddr -> Visit -> TaggedT t m [PcEnv]
getArcPcEnvs pred_ visit = do
    r <- runExceptT $ do
        prevs <- lift $ askPrevs visit >>= pruneVisits . filter (\prev -> prev.nodeId == Addr pred_)
        ensureM $ length prevs <= 1
        fmap catMaybes $ for prevs $ \prev -> do
            checkGenerality prev
            lift $ getArcPcEnv prev visit
    case r of
        Right x -> return x
        Left (TooGeneral { split }) ->
            concat <$> traverse (getArcPcEnvs pred_) (specialize visit split)

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
        runExceptT (lift (askPrevs curVisit >>= pruneVisits) >>= filterM f) >>= \case
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
                        ExprValueVar name -> do
                            return $ env ! name
                        _ -> do
                            let name = localName visit update.var.name
                            withEnv env (flattenExpr update.val) >>= fmap varFromNameTyE . liftInner . addDef name
                    return (update.var.name, val)
                return [(basicNode.next, PcEnv pc (M.union (M.fromList updates) env))]
            NodeCond condNode -> do
                cond <- withEnv env (flattenExpr condNode.expr) >>= fmap varFromNameTyE . liftInner . addDef (condName visit)
                let lpc = andE cond pc
                let rpc = andE (notE cond) pc
                return [(condNode.left, PcEnv lpc env), (condNode.right, PcEnv rpc env)]
            NodeCall callNode -> do
                preHook <- askHook #preEmitCallNodeHook
                preHook visit
                success <- liftInner $ varFromNameTyE <$>
                    addVar (successName visit callNode.functionName) boolT
                ins <- for callNode.input $ withEnv env . flattenExpr
                memCalls <- fmap (addMemCall callNode.functionName) <$> scanMemCallsHere ins
                env' <- addVarReps
                    (VarRepContextCall { funName = callNode.functionName })
                    (localName visit)
                    memCalls
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

instEqWithEnvs :: TaggedC t n m => (GraphExpr, ExprEnv) -> (GraphExpr, ExprEnv) -> m FlatExpr
instEqWithEnvs (x, xenv) (y, yenv) = do
    x' <- withEnv xenv $ flattenExpr x
    y' <- withEnv yenv $ flattenExpr y
    let f = case x'.ty of
            ExprTypeRelWrapper -> applyRelWrapper
            _ -> eqE
    return $ f x' y'

addPValidDomAssertions :: Monad m => m ()
addPValidDomAssertions = do
    ensureM cheatMemDoms
    return ()

--

scanMemCallsHere :: TaggedC t n m => [FlatExpr] -> m (Maybe MemCalls)
scanMemCallsHere = scanMemCalls $ \name -> liftInner (lookupDef name) >>= \case
    Just def -> return $ Right def
    Nothing -> Left <$> do
        liftPure $ use $ #memCalls % at name

data FunCallInfo
  = FunCallInfo
      { ins :: [FlatExpr]
      , outs :: [FlatExpr]
      , success :: FlatExpr
      }
  deriving (Eq, Generic, Ord, Show)

getFunCallInfoRawOpt :: C t m => Visit -> TaggedT t m (Maybe FunCallInfo)
getFunCallInfoRawOpt visit = do
    key <- askWithTag visit
    liftPure $ use $ #funCalls % at key

getFunCallInfoRaw :: C t m => Visit -> TaggedT t m FunCallInfo
getFunCallInfoRaw visit = fromJust <$> getFunCallInfoRawOpt visit

getFunCallInfoRawByTag :: C t m => ByTag t Visit -> T t m (ByTag t FunCallInfo)
getFunCallInfoRawByTag visits = for (withTags visits) $ \(WithTag tag visit) ->
    runTagged tag $ getFunCallInfoRaw visit

getFunCallInfo :: (C t m, MonadError TooGeneral m) => Visit -> TaggedT t m FunCallInfo
getFunCallInfo unprunedVisit = do
    visit <- fromJust <$> pruneVisit unprunedVisit
    node <- askNode $ nodeAddrOf visit.nodeId
    ensureM $ is #_NodeCall node
    infoOpt <- getFunCallInfoRawOpt visit
    whenNothing infoOpt $ do
        askCont visit >>= getNodePcEnv
        getFunCallInfoRaw visit

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
    funName <- lift $ liftUntagged $ WithTag tag <$> askFunName visit
    pairingIdOpt <- gview $ #pairingsAccess % at funName
    for_ pairingIdOpt $ \pairingId -> do
        let otherFunName = viewAtTag (otherTag tag) (withTags pairingId)
        group <- lift $ liftUntagged $ liftPure $ use $ #funCallsByName % to (M.findWithDefault [] otherFunName)
        for_ group $ \otherVisit -> do
            let visits = byTagFrom $ \tag' -> if tag' == tag then visit else otherVisit
            compat <- mapReaderT liftUntagged $ areFunCallsCompatible visits
            when compat $ do
                imp <- mapReaderT liftUntagged $ getFunAssert visits
                lift $ liftInner $ assertFlatExpr $ weakenAssert imp

areFunCallsCompatible :: AsmRefineC t m => ByTag' Visit -> ReaderT (AddFunAssertHookEnv t) (T t m) Bool
areFunCallsCompatible visits = do
    lowLevelInfoByTag <- lift $ getFunCallInfoRawByTag visits
    memCallsOpt <- for lowLevelInfoByTag $ \lowLevelInfo -> lift $ scanMemCallsHere lowLevelInfo.ins
    lookupSig <- gview #lookupSig
    pairingsAccess <- gview $ #pairingsAccess
    let lookupPairingId k = pairingsAccess ! k
    return $ areMemCallsCompatible lookupSig lookupPairingId memCallsOpt

getFunAssert :: RefineC t m => ByTag t Visit -> ReaderT (AddFunAssertHookEnv t) (T t m) FlatExpr
getFunAssert visits = do
    pairingId <- lift $ traverse askFunName visits
    pairing <- gview $ #pairings % #unwrap % expectingAt pairingId
    lookupSig <- gview #lookupSig
    lift $ do
        let sigs = lookupSig <$> withTags pairingId
        lowLevelInfo <- getFunCallInfoRawByTag visits
        let info = augmentFunCallInfo <$> sigs <*> lowLevelInfo
        pcs <- traverse getPcWithTag (withTags visits)
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

asmRefineIsStackHook :: WithTag' Ident -> PairingEqDirection -> NameTy -> Bool
asmRefineIsStackHook (WithTag tag _funName) _direction var = and
    [ tag == Asm
    , var.name == Ident "stack"
    , var.ty == ExprTypeMem
    ]

asmRefineStackPointerHook :: ArgRenames AsmRefineTag -> AsmRefineTag -> Maybe GraphExpr
asmRefineStackPointerHook argRenames tag = case tag of
    Asm ->
        let name = argRenames
                (PairingEqSideQuadrant tag PairingEqDirectionIn)
                (Ident "r13")
         in Just $ varE machineWordT name
    _ -> Nothing
