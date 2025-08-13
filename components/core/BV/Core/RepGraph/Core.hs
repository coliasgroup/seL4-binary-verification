{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}

-- TODO
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module BV.Core.RepGraph.Core
    ( ForTag
    , MemCalls
    , MemCallsForFunction (..)
    , MonadRepGraph (..)
    , MonadRepGraphDefaultHelper (..)
    , MonadRepGraphForTag (..)
    , RepGraphEnv
    , RepGraphState
    , TooGeneral (..)
    , VarRepRequestKind (..)
    , askCont
    , askLoopData
    , askNodeGraph
    , askProblem
    , askWithTag
    , convertInnerExprWithPcEnv
    , getFunc
    , getFuncRaw
    , getInductVar
    , getNodePcEnv
    , getNodePcEnvWithTag
    , getPc
    , getPcWithTag
    , initRepGraph
    , initRepGraphEnv
    , initRepGraphState
    , instEqWithEnvs
    , mapForTag
    , runForTag
    , scanMemCalls
    , scanMemCallsEnv
    , substInduct
    , tryGetNodePcEnv
    , zeroMemCallsForFunction
    ) where

import BV.Core.GenerateFreshName (generateFreshName)
import BV.Core.Logic
import BV.Core.RepGraph.Solver
import BV.Core.Structs (MonadStructs)
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils
import BV.SMTLIB2.SExpr (GenericSExpr (List))
import BV.Utils

import Control.DeepSeq (NFData)
import Control.Monad (filterM, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (Reader, ReaderT (runReaderT), ask, mapReaderT)
import Control.Monad.RWS (MonadState (get), MonadWriter (..), RWST)
import Control.Monad.State (StateT (runStateT), execStateT, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), hoistMaybe, runMaybeT)
import Data.Char (isAlpha)
import Data.Foldable (for_, toList, traverse_)
import Data.List (intercalate, sort, tails)
import Data.List.Split (splitOn)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing, mapMaybe)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))
import Text.Printf (printf)
import Data.Functor (void)

-- TODO cache more accross groups?

class MonadRepGraph t n => MonadRepGraphDefaultHelper t n m | m -> t, m -> n where
    liftMonadRepGraphDefaultHelper :: n a -> m a
    default liftMonadRepGraphDefaultHelper :: (m ~ t' n, MonadTrans t') => n a -> m a
    liftMonadRepGraphDefaultHelper = lift

class (Tag t, MonadRepGraphSolver m) => MonadRepGraph t m | m -> t where
    liftRepGraph :: StateT (RepGraphState t) (Reader (RepGraphEnv t)) a -> m a
    runProblemVarRepHook :: NameTy -> VarRepRequestKind -> NodeAddr -> ForTag t m (Maybe Expr)
    runPostEmitNodeHook :: Visit -> m ()
    runPreEmitCallNodeHook :: Visit -> Expr -> ExprEnv -> ForTag t m ()
    runPostEmitCallNodeHook :: Visit -> ForTag t m ()

    default liftRepGraph :: MonadRepGraphDefaultHelper t n m => StateT (RepGraphState t) (Reader (RepGraphEnv t)) a -> m a
    liftRepGraph = liftMonadRepGraphDefaultHelper . liftRepGraph

    default runProblemVarRepHook :: MonadRepGraphDefaultHelper t n m => NameTy -> VarRepRequestKind -> NodeAddr -> ForTag t m (Maybe Expr)
    runProblemVarRepHook = compose3 (mapForTag liftMonadRepGraphDefaultHelper) runProblemVarRepHook

    default runPostEmitNodeHook :: MonadRepGraphDefaultHelper t n m => Visit -> m ()
    runPostEmitNodeHook = liftMonadRepGraphDefaultHelper . runPostEmitNodeHook

    default runPreEmitCallNodeHook :: MonadRepGraphDefaultHelper t n m => Visit -> Expr -> ExprEnv -> ForTag t m ()
    runPreEmitCallNodeHook = compose3 (mapForTag liftMonadRepGraphDefaultHelper) runPreEmitCallNodeHook

    default runPostEmitCallNodeHook :: MonadRepGraphDefaultHelper t n m => Visit -> ForTag t m ()
    runPostEmitCallNodeHook = mapForTag liftMonadRepGraphDefaultHelper . runPostEmitCallNodeHook

-- TODO
-- instance (MonadTrans t, MonadRepGraph t m) => MonadRepGraphDefaultHelper m (t m) where
--     liftMonadRepGraphDefaultHelper = lift

instance MonadRepGraph t m => MonadRepGraphDefaultHelper t m (ReaderT r m)

instance MonadRepGraph t m => MonadRepGraphDefaultHelper t m (StateT s  m)

instance (Monoid w, MonadRepGraph t m) => MonadRepGraphDefaultHelper t m (RWST r w s m)

instance MonadRepGraph t m => MonadRepGraphDefaultHelper t m (MaybeT m)

instance MonadRepGraph t m => MonadRepGraphDefaultHelper t m (ExceptT e m)

instance MonadRepGraph t m => MonadRepGraph t (ReaderT r m)

instance MonadRepGraph t m => MonadRepGraph t (StateT s m)

instance (Monoid w, MonadRepGraph t m) => MonadRepGraph t (RWST r w s m)

instance MonadRepGraph t m => MonadRepGraph t (MaybeT m)

instance MonadRepGraph t m => MonadRepGraph t (ExceptT e m)

instance MonadRepGraph t m => MonadRepGraphDefaultHelper t m (ForTag t m)

data RepGraphEnv t
  = RepGraphEnv
      { problem :: Problem t
      , analysis :: ProblemAnalysis t
      }
  deriving (Generic)

data RepGraphState t
  = RepGraphState
      { inpEnvs :: Map NodeId ExprEnv
      , memCalls :: Map SExprWithPlaceholders MemCalls
      , nodePcEnvs :: Map (WithTag t Visit) (Maybe PcEnv)
      , arcPcEnvs :: Map (WithTag t Visit) (Map NodeId PcEnv)
      , inductVarEnv :: Map EqHypInduct Name
      , contractions :: Map SExprWithPlaceholders MaybeSplit
      , extraProblemNames :: S.Set Ident
      , hasInnerLoop :: Map (WithTag t NodeAddr) Bool
      , funcs :: M.Map (WithTag t Visit) ([(ExprType, MaybeSplit)], [(ExprType, MaybeSplit)], Expr)
      }
  deriving (Eq, Generic, NFData)

data TooGeneral
  = TooGeneral
      { split :: NodeAddr
      }
  deriving (Eq, Generic, Ord, Show)

initRepGraphEnv :: Tag t => Problem t -> RepGraphEnv t
initRepGraphEnv problem = RepGraphEnv
    { problem
    , analysis = analyzeProblem problem
    }

initRepGraphState :: RepGraphState t
initRepGraphState = RepGraphState
    { inpEnvs = M.empty
    , memCalls = M.empty
    , nodePcEnvs = M.empty
    , arcPcEnvs = M.empty
    , inductVarEnv = M.empty
    , contractions = M.empty
    , extraProblemNames = S.empty
    , hasInnerLoop = M.empty
    , funcs = M.empty
    }

initRepGraph :: MonadRepGraph t m => m ()
initRepGraph = do
    addInputEnvs

--
class MonadRepGraph t m => MonadRepGraphForTag t m where
    askTag :: m t

instance MonadRepGraphForTag t m => MonadRepGraphForTag t (ReaderT r m) where
    askTag = lift askTag

instance MonadRepGraphForTag t m => MonadRepGraphForTag t (StateT s m) where
    askTag = lift askTag

instance (Monoid w, MonadRepGraphForTag t m) => MonadRepGraphForTag t (RWST r w s m) where
    askTag = lift askTag

instance MonadRepGraphForTag t m => MonadRepGraphForTag t (MaybeT m) where
    askTag = lift askTag

instance MonadRepGraphForTag t m => MonadRepGraphForTag t (ExceptT e m) where
    askTag = lift askTag

newtype ForTag t m a
  = ForTag { run :: ReaderT t m a }
  deriving (Functor, Generic)
  deriving newtype
    ( Applicative
    , Monad
    , MonadError e
    , MonadRepGraph t
    , MonadRepGraphSolver
    , MonadRepGraphSolverSend
    , MonadStructs
    , MonadTrans
    , MonadWriter w
    )

instance MonadRepGraph t m => MonadRepGraphForTag t (ForTag t m) where
    askTag = ForTag ask

joinForTag :: MonadRepGraphForTag t m => ForTag t m a -> m a
joinForTag m = do
    tag <- askTag
    runForTag tag m

runForTag :: Monad m => t -> ForTag t m a -> m a
runForTag tag m = runReaderT m.run tag

mapForTag :: (m a -> n b) -> ForTag t m a -> ForTag t n b
mapForTag = over #run . mapReaderT

askWithTag :: MonadRepGraphForTag t m => a -> m (WithTag t a)
askWithTag a = do
    tag <- askTag
    return $ WithTag tag a

--

withMapSlot :: (MonadRepGraph t m, Ord k) => Lens' (RepGraphState t) (M.Map k v) -> k -> m v -> m v
withMapSlot l k m = do
    opt <- liftRepGraph (use (l % at k))
    whenNothing opt $ do
        v <- m
        liftRepGraph $ l %= M.insertWith (error "unexpected") k v
        return v

withMapSlotForTag :: (MonadRepGraphForTag t m, Ord k) => Lens' (RepGraphState t) (M.Map (WithTag t k) v) -> k -> m v -> m v
withMapSlotForTag l k m = do
    k' <- askWithTag k
    withMapSlot l k' m

--

askProblem :: MonadRepGraph t m => m (Problem t)
askProblem = liftRepGraph $ gview #problem

askNode :: MonadRepGraph t m => NodeAddr -> m Node
askNode addr = liftRepGraph $ gview $ #problem % #nodes % at addr % unwrapped

askNodeGraph :: MonadRepGraph t m => m NodeGraph
askNodeGraph = liftRepGraph $ gview $ #analysis % #nodeGraph

askIsNonTriviallyReachableFrom :: MonadRepGraph t m => NodeAddr -> NodeId -> m Bool
askIsNonTriviallyReachableFrom from to_ = do
    g <- liftRepGraph $ gview $ #analysis % #nodeGraph
    fromNode <- askNode from
    return $ or [ isReachableFrom g fromCont to_ | fromCont <- fromNode ^.. nodeConts ]

askLoopData :: MonadRepGraphForTag t m => m LoopData
askLoopData = liftRepGraph $ gview $ #analysis % #loopData

askLoopHead :: MonadRepGraphForTag t  m => NodeAddr -> m (Maybe NodeAddr)
askLoopHead n = loopHeadOf n <$> askLoopData

askLoopBody :: MonadRepGraphForTag t m => NodeAddr -> m (S.Set NodeAddr)
askLoopBody n = loopBodyOf n <$> askLoopData

askLoopContaining :: MonadRepGraphForTag t m => NodeAddr -> m Loop
askLoopContaining n = fromJust . flip loopContainingOf n <$> askLoopData

askPreds :: MonadRepGraphForTag t m => NodeId -> m (Set NodeAddr)
askPreds n = do
    tag <- askTag
    liftRepGraph $ gview $ #analysis % #preds % atTag tag % to ($ n)

askPrevs :: MonadRepGraphForTag t m => Visit -> m [Visit]
askPrevs visit = do
    preds <- toList <$> askPreds visit.nodeId
    let f pred_ = Visit (Addr pred_) <$> incrVCs visit.restrs pred_ (-1)
    return $ mapMaybe f preds

askPrevsPruned :: (MonadRepGraphForTag t m, MonadError TooGeneral m) => Visit -> m [Visit]
askPrevsPruned visit = do
    unprunedPrevs <- askPrevs visit
    prevs <- catMaybes <$> traverse pruneVisit unprunedPrevs
    traverse_ checkGenerality prevs
    return prevs

askCont :: MonadRepGraph t m => Visit -> m Visit
askCont visit = do
    let addr = nodeAddrOf visit.nodeId
    conts <- toListOf nodeConts <$> askNode addr
    let [cont] = conts
    return $ Visit
        { nodeId = cont
        , restrs = fromJust $ incrVCs visit.restrs addr 1
        }

incrVCs :: [Restr] -> NodeAddr -> Integer -> Maybe [Restr]
incrVCs vcount n incr = if
    | n `M.notMember` m -> Just vcount
    | isEmptyVC vcNew -> Nothing
    | otherwise -> Just (fromMapVC (M.insert n vcNew m))
  where
    m = toMapVC vcount
    vcOld = m ! n
    vcNew = incrVC incr vcOld

getHasInnerLoop :: MonadRepGraphForTag t m => NodeAddr -> m Bool
getHasInnerLoop loopHead = withMapSlotForTag #hasInnerLoop loopHead $ do
    p <- liftRepGraph $ gview #problem
    loop <- askLoopContaining loopHead
    return $ not $ null $ innerLoopsOf p.nodes loop

getFreshIdent :: MonadRepGraph t m => NameHint -> m Ident
getFreshIdent nameHint = do
    problemNames <- liftRepGraph $ gview $ #analysis % #varNames
    extraProblemNames <- liftRepGraph $ use #extraProblemNames
    let taken n = S.member n problemNames || S.member n extraProblemNames
    let n = Ident $ generateFreshName (taken . Ident) nameHint
    liftRepGraph $ #extraProblemNames %= S.insert n
    return n

--

localNameBefore :: Ident -> Visit -> NameHint
localNameBefore s vis = printf "%s_v_at_%s" s.unwrap (nodeCountName vis)

localName :: Ident -> Visit -> NameHint
localName s vis = printf "%s_after_%s" s.unwrap (nodeCountName vis)

condName :: Visit -> NameHint
condName  vis = printf "cond_at_%s" (nodeCountName vis)

pathCondName :: MonadRepGraphForTag t m => Visit -> m NameHint
pathCondName visit = do
    tag <- askTag
    return $ printf "path_cond_to_%s_%s" (nodeCountName visit) (prettyTag tag)

successName :: Ident -> Visit -> NameHint
successName fname vis =
    printf "%s_success_at_%s" name (nodeCountName vis)
  where
    name = case reverse names of
        [] -> "fun"
        name':_ -> name'
    names =
        [ intercalate "_" bits'
        | bits' <- filter (not . null) $ tails bits
        , all isAlpha (head bits')
        ]
    bits = splitOn "." fname.unwrap

nodeCountName :: Visit -> NameHint
nodeCountName vis = intercalate "_" $
    [ prettyNodeId vis.nodeId
    ] ++
    [ printf "%s=%s" (prettyNodeId (Addr restr.nodeAddr)) (visitCountName restr.visitCount)
    | restr <- vis.restrs
    ]

-- TODO will not match python
visitCountName :: VisitCount -> String
visitCountName (VisitCount { numbers, offsets }) =
    intercalate "_" $ map showNumber numbers ++ map showOffset offsets
  where
    showNumber = show
    showOffset n = "i+" ++ show n

--

type MemCalls = Map Ident MemCallsForFunction

data MemCallsForFunction
  = MemCallsForFunction
      { min :: Integer
      , max :: Maybe Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

zeroMemCallsForFunction :: MemCallsForFunction
zeroMemCallsForFunction = MemCallsForFunction
    { min = 0
    , max = Just 0
    }

addMemCall :: Ident -> Maybe MemCalls -> Maybe MemCalls
addMemCall fname = fmap $ flip M.alter fname $ \slot -> Just $
    let f = (#min %~ (+1 )) . (#max % _Just %~ (+1 ))
     in f $ fromMaybe zeroMemCallsForFunction slot

getMemCalls :: MonadRepGraph t m => SExprWithPlaceholders -> m MemCalls
getMemCalls sexpr = do
    memCallsOpt <- liftRepGraph $ use $ #memCalls % at sexpr
    whenNothing memCallsOpt $ do
        case sexpr of
            List [op, x, _, _] | isStore op -> getMemCalls x
            List [op, _, x, y] | op == symbolS "ite" -> mergeMemCalls <$> getMemCalls x <*> getMemCalls y
            _ -> do
                r <- runMaybeT $ do
                    name <- hoistMaybe $ parseSymbolS sexpr
                    next <- MaybeT $ tryGetDef $ Name name
                    getMemCalls next
                whenNothing r $ do
                    error $ "getMemCalls fallthrough: " ++ show (showSExprWithPlaceholders sexpr)
  where
    isStore s = s `elem`
        ([ symbolS "store-word8"
         , symbolS "store-word32"
         , symbolS "store-word64"
         ] :: [SExprWithPlaceholders])

scanMemCallsEnv :: MonadRepGraph t m => ExprEnv -> m (Maybe MemCalls)
scanMemCallsEnv env = scanMemCalls
    [ (var.ty, v)
    | (var, v) <- M.toList env
    ]

scanMemCalls :: MonadRepGraph t m => [(ExprType, MaybeSplit)] -> m (Maybe MemCalls)
scanMemCalls tyVals = do
    let vals = [ v | (ty, v) <- tyVals, ty == memT ]
    memCalls <- traverse getMemCalls (vals ^.. folded % #_NotSplit)
    return $ case memCalls of
        [] -> Nothing
        _ -> Just $ foldr1 mergeMemCalls memCalls

addLoopMemCalls :: MonadRepGraphForTag t m => NodeAddr -> Maybe MemCalls -> m (Maybe MemCalls)
addLoopMemCalls split = traverse $ \memCalls -> do
    loopBody <- askLoopBody split
    fnames <- fmap (S.fromList . catMaybes) $ for (toList loopBody) $ \n -> do
        node <- askNode n
        return $ case node of
            NodeCall callNode -> Just callNode.functionName
            _ -> Nothing
    let newMemCalls = flip M.fromSet fnames $ \fname -> case M.lookup fname memCalls of
                Just x -> x & #max .~ Nothing
                Nothing -> MemCallsForFunction 0 Nothing
    return $ M.union newMemCalls memCalls

mergeMemCalls :: MemCalls -> MemCalls -> MemCalls
mergeMemCalls xcalls ycalls =
    if xcalls == ycalls
    then xcalls
    else
        let ks = S.union (M.keysSet xcalls) (M.keysSet ycalls)
         in flip M.fromSet ks $ \k -> f
                (fromMaybe zeroMemCallsForFunction (M.lookup k xcalls))
                (fromMaybe zeroMemCallsForFunction (M.lookup k ycalls))
  where
    f x y = MemCallsForFunction
        { min = min x.min y.min
        , max = liftA2 max x.max y.max
        }

--

-- TODO
addLocalDef :: MonadRepGraph t m => () -> () -> NameHint -> Expr -> ReaderT ExprEnv m MaybeSplit
addLocalDef _ _ = addDef

addVarRestrWithMemCalls :: MonadRepGraph t m => NameHint -> ExprType -> Maybe MemCalls -> m Name
addVarRestrWithMemCalls nameHint ty memCallsOpt = do
    r <- addVarRestr nameHint ty
    when (isMemT ty) $ do
        liftRepGraph $ #memCalls %= M.insert (nameS r) (fromJust memCallsOpt)
    return r

--

contract :: MonadRepGraph t m => Visit -> NameTy -> SExprWithPlaceholders -> m MaybeSplit
contract visit var sexpr = withMapSlot #contractions sexpr $ do
    let name' = localNameBefore var.name visit
    withoutEnv $ addDef name' (smtExprE var.ty (NotSplit sexpr))

maybeContract :: MonadRepGraph t m => Visit -> NameTy -> MaybeSplit -> m MaybeSplit
maybeContract visit var v = case v of
    NotSplit sexpr | length (showSExprWithPlaceholders sexpr) > 80 -> contract visit var sexpr
    _ -> return v

specialize :: Visit -> NodeAddr -> [[Restr]]
specialize visit split = ensure (isOptionsVC vc)
    [ fromMapVC $ M.insert split (fromSimpleVC n) m
    | n <- enumerateSimpleVC vc
    ]
  where
    m = toMapVC visit.restrs
    vc = m ! split

--

data VarRepRequestKind
  = VarRepRequestKindCall
  | VarRepRequestKindInit
  | VarRepRequestKindLoop
  deriving (Eq, Generic, Ord, Show)

varRepRequest :: MonadRepGraphForTag t m => NameTy -> VarRepRequestKind -> Visit -> ExprEnv -> m (Maybe SplitMem)
varRepRequest var kind visit env = runMaybeT $ do
    let n = nodeAddrOf visit.nodeId
    addrExpr <- MaybeT $ joinForTag $ runProblemVarRepHook var kind n
    addrSexpr <- withEnv env $ convertExpr addrExpr
    let name' = printf "%s_for_%s" var.name.unwrap (nodeCountName visit)
    addSplitMemVar (addrSexpr ^. expecting #_NotSplit) name' var.ty

--

addInputEnvs :: MonadRepGraph t m => m ()
addInputEnvs = do
    p <- askProblem
    traverse_ f (withTags p.sides)
  where
    f (WithTag tag side) = runForTag tag $ do
        env <- flip execStateT M.empty $ do
            for_ side.input $ \arg -> do
                var <- addVarRestrWithMemCalls
                    (arg.name.unwrap ++ "_init")
                    arg.ty
                    (Just M.empty)
                modify $ M.insert arg (NotSplit (nameS var))
            for_ side.input $ \arg -> do
                env <- get
                opt <- varRepRequest
                    arg
                    VarRepRequestKindInit
                    (Visit { nodeId = side.entryPoint, restrs = []})
                    env
                for_ opt $ \splitMem -> modify $ M.insert arg (Split splitMem)
        liftRepGraph $ #inpEnvs %= M.insert side.entryPoint env

getPcWithTag :: MonadRepGraph t m => WithTag t Visit -> m Expr
getPcWithTag (WithTag tag visit) = runForTag tag $ getPc visit

getPc :: MonadRepGraphForTag t m => Visit -> m Expr
getPc visit = getNodePcEnv visit >>= \case
    Nothing -> return falseE
    Just (PcEnv pc env) -> withEnv env $ convertInnerExpr pc

getInductVar :: MonadRepGraph t m => EqHypInduct -> m Expr
getInductVar induct =
    fmap (smtExprE word32T . NotSplit . nameS) $
        withMapSlot #inductVarEnv induct $
            addVar (printf "induct_i_%d_%d" induct.a induct.b) word32T

substInduct :: Expr -> Expr -> Expr
substInduct expr inductVar = varSubst f expr
  where
    f (NameTy { name = Ident "%n", ty = ExprTypeWord 32 }) = Just inductVar
    f _ = Nothing

instEqWithEnvs :: MonadRepGraphSolver m => (Expr, ExprEnv) -> (Expr, ExprEnv) -> m Expr
instEqWithEnvs (x, xenv) (y, yenv) = do
    x' <- withEnv xenv $ convertUnderOp x
    y' <- withEnv yenv $ convertUnderOp y
    let f = case x'.ty of
            ExprTypeRelWrapper -> applyRelWrapper
            _ -> eqE
    return $ f x' y'
  where
    convertUnderOp :: MonadRepGraphSolver m => Expr -> ReaderT ExprEnv m Expr
    convertUnderOp expr = case expr.value of
        ExprValueOp op args -> do
            args' <- traverse convertInnerExpr args
            return $ Expr expr.ty $ ExprValueOp op args'
        _ -> convertInnerExpr expr

--

getNodePcEnvWithTag :: MonadRepGraph t m => WithTag t Visit -> m (Maybe PcEnv)
getNodePcEnvWithTag (WithTag tag visit) = runForTag tag $ getNodePcEnv visit

getNodePcEnv :: MonadRepGraphForTag t m => Visit -> m (Maybe PcEnv)
getNodePcEnv visit = view (expecting _Right) <$> runExceptT (tryGetNodePcEnv visit)

tryGetNodePcEnv :: (MonadRepGraphForTag t m, MonadError TooGeneral m) => Visit -> m (Maybe PcEnv)
tryGetNodePcEnv unprunedVisit = runMaybeT $ do
    visit <- MaybeT $ pruneVisit unprunedVisit
    MaybeT $ withMapSlotForTag #nodePcEnvs visit $ do
        warmPcEnvCache visit
        getNodePcEnvRaw visit

getNodePcEnvRaw :: MonadRepGraphForTag t m => Visit -> m (Maybe PcEnv)
getNodePcEnvRaw visit = do
    liftRepGraph (use $ #inpEnvs % at visit.nodeId) >>= \case
        Just env -> return $ Just $ PcEnv trueE env
        Nothing -> do
            let f restr = Addr restr.nodeAddr == visit.nodeId && restr.visitCount == offsetVC 0
            if any f visit.restrs
                then getLoopPcEnv visit
                else do
                    pcEnvs <- toListOf (folded % folded % _Just) <$> do
                        preds <- askPreds visit.nodeId
                        for (toList preds) $ \pred_ -> getArcPcEnvs pred_ visit
                    case pcEnvs of
                        [] -> return Nothing
                        _ -> do
                            pcEnvs' <- case visit.nodeId of
                                Err -> for pcEnvs $ \(PcEnv pc env) -> do
                                    pc' <- withEnv env $ convertInnerExpr pc
                                    return $ PcEnv pc' M.empty
                                _ -> return pcEnvs
                            (PcEnv pc env, _large) <- mergeEnvsPcs pcEnvs'
                            pc' <- case pc.value of
                                ExprValueSMTExpr _ -> return pc
                                _ -> do
                                    hint <- pathCondName visit
                                    name <- withEnv env $ addDef hint pc
                                    return $ smtExprE boolT name
                            Just . PcEnv pc' <$> M.traverseWithKey (maybeContract visit) env

getLoopPcEnv :: MonadRepGraphForTag t m => Visit -> m (Maybe PcEnv)
getLoopPcEnv visit = do
    prevPcEnvOpt <- getNodePcEnv $ visit & #restrs %~ withMapVC (M.insert visitAddr (numberVC 0))
    for prevPcEnvOpt $ \prevPcEnv -> do
        memCalls <- scanMemCallsEnv prevPcEnv.env >>= addLoopMemCalls visitAddr
        let add name ty = do
                let hint = printf "%s_loop_at_%s" name (prettyNodeId visit.nodeId)
                addVarRestrWithMemCalls hint ty memCalls
        (env, consts) <- flip runStateT S.empty $ flip M.traverseWithKey prevPcEnv.env $ \var _v -> do
            let checkConst = case var.ty of
                    ExprTypeHtd -> True
                    ExprTypeDom -> True
                    _ -> False
            isSyntConst <- if checkConst then isSyntacticConstant var visitAddr else return False
            if isSyntConst
                then do
                    modify $ S.insert var
                    return $ prevPcEnv.env ! var
                else do
                    NotSplit . nameS <$> lift (add (var.name.unwrap ++ "_after") var.ty)
        env' <- flip M.traverseWithKey env $ \var v -> do
            if S.member var consts
                then return v
                else maybe v Split <$> varRepRequest var VarRepRequestKindLoop visit env
        pc' <- smtExprE boolT . NotSplit . nameS <$> add "pc_of" boolT
        return $ PcEnv pc' env'
  where
    visitAddr = nodeAddrOf visit.nodeId

getArcPcEnvs :: MonadRepGraphForTag t m => NodeAddr -> Visit -> m [Maybe PcEnv]
getArcPcEnvs pred_ visit = do
    r <- runExceptT $ do
        prevs <- filter (\prev -> prev.nodeId == Addr pred_) <$> askPrevs visit
        ensureM $ length prevs <= 1
        for prevs $ \prev -> runMaybeT $ do
            prunedPrev <- MaybeT $ pruneVisit prev
            checkGenerality prunedPrev
            MaybeT $ getArcPcEnv prunedPrev visit
    case r of
        Right x -> return x
        Left (TooGeneral { split }) ->
            concat <$> traverse (getArcPcEnvs pred_ . Visit visit.nodeId) (specialize visit split)

getArcPcEnv :: MonadRepGraphForTag t m => Visit -> Visit -> m (Maybe PcEnv)
getArcPcEnv prev visit = runMaybeT $ do
    key <- askWithTag prev
    opt <- liftRepGraph $ use $ #arcPcEnvs % at key
    case opt of
        Just r -> hoistMaybe $ r !? visit.nodeId
        Nothing -> do
            MaybeT $ getNodePcEnv prev
            arcs <- M.fromList <$> emitNode prev
            runPostEmitNodeHook prev
            liftRepGraph $ #arcPcEnvs %= M.insert key arcs
            hoistMaybe $ arcs !? visit.nodeId

pruneVisit :: MonadRepGraphForTag t m => Visit -> m (Maybe Visit)
pruneVisit visit = do
    restrsWithReachability <- for visit.restrs $ \restr ->
        (restr,) <$> askIsNonTriviallyReachableFrom restr.nodeAddr visit.nodeId
    return $
        if flip any restrsWithReachability $ \(restr, reachable) ->
                not reachable && not (hasZeroVC restr.visitCount)
        then Nothing
        else Just $
                visit & #restrs .~
                    sort [ restr | (restr, reachable) <- restrsWithReachability, reachable ]

checkGenerality :: (MonadRepGraphForTag t m, MonadError TooGeneral m) => Visit -> m ()
checkGenerality visit = void $ runMaybeT $ do
    nodeAddr <- hoistMaybe $ preview #_Addr visit.nodeId
    loopId <- MaybeT $ askLoopHead nodeAddr
    for_ visit.restrs $ \restr -> do
        loopIdOpt' <- askLoopHead restr.nodeAddr
        when (loopIdOpt' == Just loopId && isOptionsVC restr.visitCount) $ do
            throwError $ TooGeneral { split = restr.nodeAddr }

warmPcEnvCache :: MonadRepGraphForTag t m => Visit -> m ()
warmPcEnvCache visit = go iters [] visit >>= traverse_ getNodePcEnv
  where
    go 0 prevChain _ = return prevChain
    go i prevChain curVisit = do
        let f prev = do
                key <- askWithTag prev
                present <- liftRepGraph $ use $ #nodePcEnvs % to (M.member key)
                return $ not present && prev.restrs == curVisit.restrs
        runExceptT (askPrevsPruned curVisit >>= filterM f) >>= \case
            Right (v:_) -> go (i - 1) (v:prevChain) v
            _ -> return prevChain
    iters = 5000 :: Integer

emitNode :: MonadRepGraphForTag t m => Visit -> m [(NodeId, PcEnv)]
emitNode visit = do
    pcEnv@(PcEnv pc env) <- fromJust <$> getNodePcEnv visit
    let nodeAddr = nodeAddrOf visit.nodeId
    node <- askNode nodeAddr
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
                            return $ env ! NameTy name update.val.ty
                        _ -> do
                            let name = localName update.var.name visit
                            withEnv env $ addLocalDef () () name update.val
                    return (update.var, val)
                return [(basicNode.next, PcEnv pc (M.union (M.fromList updates) env))]
            NodeCond condNode -> do
                let nameHint = condName visit
                freshName <- getFreshIdent nameHint
                let cond = varE boolT freshName
                def <- withEnv env $ addLocalDef () () nameHint condNode.expr
                let lpc = andE cond pc
                let rpc = andE (notE cond) pc
                let env' = M.insert (NameTy freshName boolT) def env
                return [(condNode.left, PcEnv lpc env'), (condNode.right, PcEnv rpc env')]
            NodeCall callNode -> do
                joinForTag $ runPreEmitCallNodeHook visit pc env
                let nameHint = successName callNode.functionName visit
                success <- smtExprE boolT . NotSplit . nameS <$> addVar nameHint boolT
                ins <- for callNode.input $ \arg -> (arg.ty,) <$> withEnv env (convertExpr arg)
                memCalls <- addMemCall callNode.functionName <$> scanMemCalls ins
                (outs, env') <- flip runStateT env $ do
                    notSplit <- for callNode.output $ \out -> do
                        var <- addVarRestrWithMemCalls (localName out.name visit) out.ty memCalls
                        modify $ M.insert out (NotSplit (nameS var))
                        return $ NotSplit (nameS var)
                    split <- for callNode.output $ \out -> do
                        opt <- get >>= varRepRequest out VarRepRequestKindCall visit
                        for opt $ \v -> do
                            modify $ M.insert out (Split v)
                            return $ Split v
                    return $ zip (map (.ty) callNode.output) $ zipWith fromMaybe notSplit split
                key <- askWithTag visit
                liftRepGraph $ #funcs %= M.insertWith (error "unexpected") key (ins, outs, success)
                joinForTag $ runPostEmitCallNodeHook visit
                return [(callNode.next, PcEnv pc env')]

isSyntacticConstant :: MonadRepGraphForTag t m => NameTy -> NodeAddr -> m Bool
isSyntacticConstant var split = do
    hasInnerLoop <- getHasInnerLoop split
    if hasInnerLoop
        then return False
        else do
            loopSet <- askLoopBody split
            let go visit safe (name, n) = do
                    node <- askNode n
                    let newName = name
                    newName' <- case node of
                        NodeCall callNode ->
                            if NameTy name var.ty `elem` callNode.output
                            then throwError False
                            else return newName
                        NodeBasic basicNode -> do
                            let updateExprs =
                                    [ u.val
                                    | u <- basicNode.varUpdates
                                    , u.var == NameTy name var.ty
                                    ]
                            case traverse (preview (#value % #_ExprValueVar)) updateExprs of
                                Nothing -> throwError False
                                Just updateExprIdents -> case updateExprIdents of
                                    name':_ -> return name'
                                    _ -> return newName
                        _ -> return newName
                    allPreds <- askPreds $ Addr n
                    let preds = [ (newName', n2) | n2 <- toList allPreds, n2 `S.member` loopSet ]
                    let unknowns = [ p | p <- preds, p `S.notMember` safe ]
                    let (visit', safe') =
                            if null unknowns
                            then (visit, S.insert (name, n) safe)
                            else (visit <> Seq.fromList [(name, n)] <> Seq.fromList unknowns, safe)
                    let f v = case unsnoc v of
                            Nothing -> throwError True
                            Just (v', hd) -> do
                                if hd `S.member` safe'
                                then f v'
                                else if snd hd == split
                                    then throwError False
                                    else go v' safe' hd
                    f visit'
            view (expecting _Left) <$> runExceptT (go mempty (S.singleton (var.name, split)) (var.name, split))

--

getFuncRaw :: MonadRepGraphForTag t m => Visit -> m ([(ExprType, MaybeSplit)], [(ExprType, MaybeSplit)], Expr)
getFuncRaw visit = do
    key <- askWithTag visit
    liftRepGraph $ use $ #funcs % at key % unwrapped

getFunc :: (MonadRepGraphForTag t m, MonadError TooGeneral m) => Visit -> m ([(ExprType, MaybeSplit)], [(ExprType, MaybeSplit)], Expr)
getFunc unprunedVisit = do
    visit <- fromJust <$> pruneVisit unprunedVisit
    node <- askNode (nodeAddrOf visit.nodeId)
    ensureM $ is #_NodeCall node
    key <- askWithTag visit
    opt <- liftRepGraph $ use $ #funcs % at key
    when (isNothing opt) $ do
        cont <- askCont visit
        getNodePcEnv cont
        return ()
    getFuncRaw visit

-- TODO GraphSlice.is_cont

--

convertInnerExprWithPcEnv :: MonadRepGraphForTag t m => Expr -> Visit -> m Expr
convertInnerExprWithPcEnv expr visit = do
    pcEnvOpt <- getNodePcEnv visit
    let Just (PcEnv _ env) = pcEnvOpt
    withEnv env $ convertInnerExpr expr
