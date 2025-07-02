{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module BV.Core.Stages.CompileProofChecks.RepGraph
    ( ArgRenames
    , FunctionSignature (..)
    , FunctionSignatures
    , MonadRepGraph (..)
    , RepGraphContext
    , RepGraphEnv
    , RepGraphState
    , getInductVarM
    , getNodePcEnvM'
    , getPcM'
    , initRepGraph
    , initRepGraphEnv
    , initRepGraphState
    , instEqWithEnvsM
    , substInduct
    ) where

import BV.Core.Graph
import BV.Core.Logic
import BV.Core.Stages.CompileProofChecks.Solver
import BV.Core.Stages.Utils (chooseFreshName)
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils
import BV.SMTLIB2.SExpr (GenericSExpr (List))

import Control.Applicative (asum)
import Control.DeepSeq (NFData)
import Control.Monad (filterM, guard, join, replicateM, unless, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (MonadReader)
import Control.Monad.RWS (MonadState (get, put), MonadWriter (..),
                          RWST (runRWST), asks, evalRWST)
import Control.Monad.State (StateT (runStateT), execStateT, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), hoistMaybe, runMaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Char (isAlpha)
import Data.Foldable (for_, toList)
import qualified Data.Graph as G
import Data.List (intercalate, isPrefixOf, nub, sort, tails)
import Data.List.Split (splitOn)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import Data.Void (Void)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))
import Text.Printf (printf)

{-# ANN module ("HLint: ignore" :: String) #-}

-- TODO cache more accross groups

type RepGraphContext m = (MonadReader RepGraphEnv m, MonadState RepGraphState m)

class MonadSolver m => MonadRepGraph m where
    liftRepGraph :: (forall n. RepGraphContext n => n a) -> m a

instance MonadRepGraph m => MonadRepGraph (ExceptT e m) where
    liftRepGraph f = lift $ liftRepGraph f

data RepGraphEnv
  = RepGraphEnv
      { functionSigs :: FunctionSignatures
      , pairings :: Pairings
      , pairingsAccess :: M.Map Ident PairingId
      , argRenames :: ArgRenames
      , problem :: Problem
      , nodeTag :: NodeAddr -> Tag
      , loopData :: Map NodeAddr LoopData
      , nodeGraph :: NodeGraph
      , preds :: Map NodeId (Set NodeAddr)
      , problemNames :: S.Set Ident
      }
  deriving (Generic)

data RepGraphState
  = RepGraphState
      { inductVarEnv :: Map EqHypInduct Name
      , nodePcEnvs :: Map VisitWithTag (Maybe (Expr, SMTEnv))
      , inpEnvs :: Map NodeId SMTEnv
      , memCalls :: Map SExprWithPlaceholders MemCalls
      , contractions :: Map SExprWithPlaceholders MaybeSplit
      , arcPcEnvs :: Map Visit (Map NodeId (Expr, SMTEnv))
      , extraProblemNames :: S.Set Ident
      , hasInnerLoop :: M.Map NodeAddr Bool
      , funcs :: M.Map Visit (SMTEnv, SMTEnv, Expr)
      , funcsByName :: M.Map (PairingOf Ident) [Visit]
        --   , knownEqs :: Map VisitWithTag [KnownEqsValue]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

-- type KnownEqsValue = ()

type FunctionSignatures = WithTag Ident -> FunctionSignature

initRepGraphEnv :: FunctionSignatures -> Pairings -> ArgRenames -> Problem -> RepGraphEnv
initRepGraphEnv functionSigs pairings argRenames problem =
    RepGraphEnv
        { functionSigs
        , pairings
        , pairingsAccess = M.fromListWith (error "unexpected") $ join [ [(p.c, p), (p.asm, p)] | p <- M.keys (pairings.unwrap)]
        , argRenames
        , problem
        -- , nodeGraph = makeNodeGraph (map (_2 %~ view #node) (M.toAscList problem.nodes))
        , nodeGraph
        , nodeTag = nodeTagOf problem nodeGraph
        , loopData = createLoopDataMap problem nodeGraph
        , preds = predsOf problem
        , problemNames = S.fromList $ toListOf varNamesOfProblem problem
        }
  where
    nodeGraph = makeNodeGraph (M.toAscList problem.nodes)

loopIdR :: MonadRepGraph m => NodeAddr -> m (Maybe NodeAddr)
loopIdR addr = liftRepGraph $ loopIdOf addr <$> gview #loopData

loopIdR' :: MonadRepGraph m => NodeId -> m (Maybe NodeAddr)
loopIdR' = \case
    Addr addr -> loopIdR addr
    _ -> return Nothing

loopHeadsR :: MonadRepGraph m => m [NodeAddr]
loopHeadsR = liftRepGraph $ loopHeadsOf <$> gview #loopData

loopBodyR :: MonadRepGraph m => NodeAddr -> m (S.Set NodeAddr)
loopBodyR n = liftRepGraph $ loopBodyOf n <$> gview #loopData

nodeTagR :: MonadRepGraph m => NodeAddr -> m Tag
nodeTagR n = liftRepGraph $ do
    gview (#nodeTag % to ($ n))

getReachableR :: MonadRepGraph m => NodeAddr -> NodeId -> m Bool
getReachableR split n = do
    g <- liftRepGraph $ gview #nodeGraph
    splitNode <- liftRepGraph $ gview $ #problem % #nodes % at split % unwrapped
    return $ any id [ isReachableFrom g splitCont n | splitCont <- splitNode ^.. nodeConts ]

predsR :: MonadRepGraph m => NodeId -> m (Set NodeAddr)
predsR n = liftRepGraph $ gview $ #preds % at n % unwrapped

prevsR :: MonadRepGraph m => Visit -> m [Visit]
prevsR visit = do
    let m = vcountToMap visit.restrs
    preds <- S.toAscList <$> predsR visit.nodeId
    let f p = Visit (Addr p) <$>
            if M.member p m
            then incrVCs visit.restrs p (-1)
            else Just visit.restrs
    return $ catMaybes $ map f preds

initRepGraphState :: RepGraphState
initRepGraphState = RepGraphState
    { inductVarEnv = M.empty
    , nodePcEnvs = M.empty
    , inpEnvs = M.empty
    , memCalls = M.empty
    , contractions = M.empty
    , arcPcEnvs = M.empty
    , extraProblemNames = S.empty
    , hasInnerLoop = M.empty
    , funcs = M.empty
    , funcsByName = M.empty
    -- , knownEqs = M.empty
    }

--

initRepGraph :: MonadRepGraph m => m ()
initRepGraph = do
    addInputEnvsM

addInputEnvsM :: MonadRepGraph m => m ()
addInputEnvsM = do
    p <- liftRepGraph $ gview #problem
    f p.sides.asm
    f p.sides.c
  where
    f side = do
        env <- mkInpEnv side.entryPoint side.input
        liftRepGraph $ #inpEnvs %= M.insert side.entryPoint env

mkInpEnv :: MonadRepGraph m => NodeId -> [Argument] -> m SMTEnv
mkInpEnv n args = flip execStateT M.empty $ do
    for_ args $ \arg -> do
        x <- lift $ addVarR (arg.name.unwrap ++ "_init") arg.ty (Just M.empty)
        modify $ M.insert (arg.name, arg.ty) (NotSplit $ nameS x)
    for_ args $ \arg -> do
        env <- get
        z <- lift $ varRepRequest arg.name arg.ty VarRepRequestKindInit (Visit { nodeId = n, restrs = []}) env
        case z of
            Nothing -> return ()
            Just z' -> do
                modify $ M.insert (arg.name, arg.ty) (Split z')

type MonadRepGraphE m = (MonadRepGraph m, MonadError TooGeneral m)

data TooGeneral
  = TooGeneral
      { split :: NodeAddr
      }
  deriving (Eq, Generic, Ord, Show)

getPcM' :: MonadRepGraph m => Visit -> Maybe Tag -> m Expr
getPcM' visit tag = view (expecting _Right) <$> runExceptT (getPcM visit tag)

getPcM :: MonadRepGraphE m => Visit -> Maybe Tag -> m Expr
getPcM visit tag = do
    pc_env <- getNodePcEnvM visit tag
    case pc_env of
        Nothing -> return falseE
        Just (pc, env) -> withEnv env $ toSmtExprM pc

toSmtExprRM :: MonadRepGraphE m => Expr -> Visit -> Maybe Tag -> m Expr
toSmtExprRM expr visit tag = do
    pc_env <- getNodePcEnvM visit tag
    let Just (_pc, env) = pc_env
    withEnv env $ toSmtExprM expr

getNodePcEnvM' :: MonadRepGraph m => Visit -> Maybe Tag -> m (Maybe (Expr, SMTEnv))
getNodePcEnvM' visit tag = view (expecting _Right) <$> runExceptT (getNodePcEnvM visit tag)

getNodePcEnvM :: MonadRepGraphE m => Visit -> Maybe Tag -> m (Maybe (Expr, SMTEnv))
getNodePcEnvM visit tag = do
    (tag', vcount) <- getTagVCount visit tag
    case vcount of
        Nothing -> return Nothing
        Just vcount' -> do
            let vt = VisitWithTag
                    { visit = Visit
                        { nodeId = visit.nodeId
                        , restrs = vcount'
                        }
                    , tag = tag'
                    }
            liftRepGraph (use (#nodePcEnvs % at vt)) >>= \case
                Just ret -> return ret
                Nothing -> do
                    warmPcEnvCacheM vt
                    pc_env <- getNodePcEnvRawM vt
                    pc_env' <- for pc_env $ \pc_env'' -> do
                        applyKnownEqsPcEnvM vt pc_env''
                    present <- liftRepGraph $ use $ #nodePcEnvs % to (M.member vt)
                    ensureM $ not present
                    liftRepGraph $ #nodePcEnvs %= M.insert vt pc_env'
                    return pc_env'

type VCount = [Restr]

vcountToMap :: [Restr] -> Map NodeAddr VisitCount
vcountToMap restrs = ensure check m
  where
    m = M.fromList [ (restr.nodeAddr, restr.visitCount) | restr <- restrs ]
    check = M.size m == length restrs

vcountFromMap :: Map NodeAddr VisitCount -> [Restr]
vcountFromMap = map f . M.toAscList
  where
    f (nodeAddr, visitCount) = Restr { nodeAddr, visitCount }

withVCountMap :: (Map NodeAddr VisitCount -> Map NodeAddr VisitCount) -> [Restr] -> [Restr]
withVCountMap f restrs = vcountFromMap (f (vcountToMap restrs))

incrVCs :: VCount -> NodeAddr -> Integer -> Maybe VCount
incrVCs vcount n incr = if isEmptyVC vc then Nothing else Just (vcountFromMap (M.insert n vc m))
  where
    m = vcountToMap vcount
    vc = incrVC incr (m ! n)

warmPcEnvCacheM :: MonadRepGraph m => VisitWithTag -> m ()
warmPcEnvCacheM visitWithTag = do
    let go = do
            n_vc <- get
            prevs <- lift $ lift $ prevsR n_vc
            let f p = do
                    present <- lift $ liftRepGraph $ use $ #nodePcEnvs % to (M.member (VisitWithTag p visitWithTag.tag))
                    if present
                        then return False
                        else do
                            vc <- getTagVCount p Nothing
                            return $ vc == (visitWithTag.tag, Just n_vc.restrs)
            prevs' <- lift $ lift $ runExceptT $ filterM f prevs
            case prevs' of
                Right (n_vc':_) -> do
                    tell [n_vc']
                    put n_vc'
                    return ()
                _ -> do
                    hoistMaybe Nothing
    (_, prevChain' :: [Visit]) <- evalRWST (runMaybeT (replicateM 5000 go)) () visitWithTag.visit
    ensureM $ length prevChain' < 5000
    let prevChain = reverse prevChain'
    for prevChain $ \n_vc -> do
        getNodePcEnvM' n_vc (Just visitWithTag.tag)
    return ()

applyKnownEqsPcEnvM :: Monad m => VisitWithTag -> (Expr, SMTEnv) -> m (Expr, SMTEnv)
applyKnownEqsPcEnvM _ pc_env = return pc_env

getTagVCount :: MonadRepGraphE m => Visit -> Maybe Tag -> m (Tag, Maybe [Restr])
getTagVCount visit mtag = do
    tag <- maybe (nodeTagR (visit.nodeId ^. expecting #_Addr)) return mtag
    vcount_r <- catMaybes <$> for visit.restrs (\restr -> runMaybeT $ do
        reachable <- lift $ getReachableR restr.nodeAddr visit.nodeId
        tag' <- lift $ nodeTagR restr.nodeAddr
        guard $ tag' == tag
        return $ (restr.nodeAddr, restr.visitCount, reachable)
        )
    let done = flip any vcount_r $ \(_split, count, r) -> not r && not (hasZeroVC count)
    if done
        then return (tag, Nothing)
        else do
            let vcount = sort [ (split, count) | (split, count, r) <- vcount_r, r ]
            -- traceM $ ("XXX " ++ nodeCountName visit ++ " -> " ++ nodeCountName (Visit visit.nodeId [ Restr x y | (x, y) <- vcount ]))
            maybeLoopId <- loopIdR' visit.nodeId
            case maybeLoopId of
                Nothing -> return ()
                Just loopId -> for_ vcount $ \(split, visits) -> do
                    maybeLoopId' <- loopIdR split
                    when (maybeLoopId' == Just loopId && isOptionsVC visits) $ do
                        throwError $ TooGeneral { split }
            return (tag, Just [ Restr x y | (x, y) <- vcount ])

getInductVarM :: MonadRepGraph m => EqHypInduct -> m Expr
getInductVarM induct = do
    vname <- liftRepGraph (use (#inductVarEnv % at induct)) >>= \case
        Just vname -> return vname
        Nothing -> do
            vname <- addVar (printf "induct_i_%d_%d" induct.a induct.b) word32T
            liftRepGraph $ #inductVarEnv %= M.insert induct (vname)
            return vname
    return $ smtExprE word32T (NotSplit $ nameS vname)

substInduct :: Expr -> Expr -> Expr
substInduct expr inductVar = flip varSubst expr $ \ident ty ->
    if ident.unwrap == "%n" && ty == word32T
    then Just inductVar
    else Nothing

toSmtExprUnderOpM :: MonadSolver m => Expr -> ReaderT SMTEnv m Expr
toSmtExprUnderOpM expr = case expr.value of
    ExprValueOp op args -> do
        args' <- mapM toSmtExprM args
        return $ expr & #value .~ ExprValueOp op args'
    _ -> toSmtExprM expr

instEqWithEnvsM :: MonadSolver m => (Expr, SMTEnv) -> (Expr, SMTEnv) -> m Expr
instEqWithEnvsM (x, env1) (y, env2) = do
    x' <- withEnv env1 $ toSmtExprUnderOpM x
    y' <- withEnv env2 $ toSmtExprUnderOpM y
    return $ case x'.ty of
        ExprTypeRelWrapper -> applyRelWrapper x' y'
        _ -> eqE x' y'

type MemCalls = Map Ident MemCallsForOne

data MemCallsForOne
  = MemCallsForOne
      { min :: Integer
      , max :: Maybe Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

zeroMemCallsForOne :: MemCallsForOne
zeroMemCallsForOne = MemCallsForOne
    { min = 0
    , max = Just 0
    }

addVarR :: MonadRepGraph m => NameHint -> ExprType -> Maybe MemCalls -> m Name
addVarR nameHint ty memCallsOpt = do
    r <- addVarRestr nameHint ty
    when (ty == ExprTypeMem) $ do
        liftRepGraph $ #memCalls %= M.insert (nameS r) (fromJust memCallsOpt)
    return r

varRepRequest :: MonadRepGraph m => Ident -> ExprType -> VarRepRequestKind -> Visit -> SMTEnv -> m (Maybe SplitMem)
varRepRequest nm typ kind n_vc env = runMaybeT $ do
    let hook = asmStackRepHook
    let n = n_vc.nodeId ^. expecting #_Addr
    addr <- MaybeT $ hook nm typ kind n
    addr_s <- lift $ withEnv env $ smtExprM addr
    let countName = nodeCountName n_vc
    let name = printf "%s_for_%s" nm.unwrap countName
    lift $ addSplitMemVar (addr_s ^. expecting #_NotSplit) name typ

data VarRepRequestKind
  = VarRepRequestKindCall
  | VarRepRequestKindInit
  | VarRepRequestKindLoop
  deriving (Eq, Generic, Ord, Show)

asmStackRepHook :: MonadRepGraph m => Ident -> ExprType -> VarRepRequestKind -> NodeAddr -> m (Maybe Expr)
asmStackRepHook nm typ kind n = runMaybeT $ do
    tag <- lift $ nodeTagR n
    guard $ tag == Asm
    guard $ "stack" `isPrefixOf` nm.unwrap
    guard $ typ == ExprTypeMem
    guard $ kind /= VarRepRequestKindInit
    argRenames <- lift $ liftRepGraph $ gview #argRenames
    return $ varE word32T (argRenames (PairingEqSideQuadrant
        { tag
        , direction = PairingEqDirectionIn
        }) (Ident "r13"))

nodeCountName :: Visit -> NameHint
nodeCountName visit = intercalate "_" $ [ prettyNodeId visit.nodeId ] ++
    [ printf "%s=%s" (prettyNodeId (Addr restr.nodeAddr)) (visitCountName restr.visitCount)
    | restr <- visit.restrs
    ]

-- TODO will not match python
visitCountName :: VisitCount -> String
visitCountName = \case
    VisitCount { numbers = [n], offsets = [] } -> showNumber n
    VisitCount { numbers = [], offsets = [n] } -> showOffset n
    VisitCount { numbers, offsets } -> intercalate "_" $ map showNumber numbers ++ map showOffset offsets
  where
    showNumber = show
    showOffset n = "i+" ++ show n

contractM :: MonadRepGraph m => Ident -> Visit -> SExprWithPlaceholders -> ExprType -> m MaybeSplit
contractM name n_vc val typ = do
    liftRepGraph (use $ #contractions % at val) >>= \case
        Just x -> return x
        Nothing -> do
            let name' = localNameBefore name n_vc
            name'' <- withoutEnv $ addDef name' (smtExprE typ (NotSplit val))
            liftRepGraph $ #contractions %= M.insert val name''
            return name''

localNameBefore :: Ident -> Visit -> NameHint
localNameBefore s n_vc = printf "%s_v_at_%s" s.unwrap (nodeCountName n_vc)

localName :: Ident -> Visit -> NameHint
localName s n_vc = printf "%s_after_%s" s.unwrap (nodeCountName n_vc)

condName :: Visit -> NameHint
condName  n_vc = printf "cond_at_%s" (nodeCountName n_vc)

pathCondName :: VisitWithTag -> NameHint
pathCondName visitWithTag = printf "path_cond_to_%s_%s" (nodeCountName visitWithTag.visit) (prettyTag visitWithTag.tag)

successName :: Ident -> Visit -> NameHint
successName fname n_vc =
    printf "%s_success_at_%s" nm (nodeCountName n_vc)
  where
    bits = splitOn "." fname.unwrap
    nms =
        [ intercalate "_" bits'
        | bits' <- filter (not . null) $ tails bits
        , all isAlpha (head bits')
        ]
    nm = case reverse nms of
        [] -> "fun"
        nm':_ -> nm'

getNodePcEnvRawM :: MonadRepGraphE m => VisitWithTag -> m (Maybe (Expr, SMTEnv))
getNodePcEnvRawM visitWithTag = do
    liftRepGraph (use $ #inpEnvs % at visitWithTag.visit.nodeId) >>= \case
        Just x -> return $ Just (trueE, x)
        Nothing -> case asum
            [ if Addr restr.nodeAddr == visitWithTag.visit.nodeId && restr.visitCount == offsetVC 0
              then Just $ getLoopPcEnvM restr.nodeAddr visitWithTag.visit.restrs
              else Nothing
            | restr <- visitWithTag.visit.restrs
            ] of
                Just m -> m
                Nothing -> do
                    pc_envs <- catMaybes . concat <$> do
                        preds <- predsR visitWithTag.visit.nodeId
                        for (S.toList preds) $ \n_prev -> do
                            tag <- nodeTagR n_prev
                            if tag /= visitWithTag.tag
                                then return []
                                else getArcPcEnvsM n_prev visitWithTag.visit
                    case pc_envs of
                        [] -> return Nothing
                        _ -> do
                            pc_envs' <- case visitWithTag.visit.nodeId of
                                Err -> do
                                    for pc_envs $ \(pc, env) -> do
                                        pc' <- withEnv env $ toSmtExprM pc
                                        return $ (pc', M.empty)
                                _ -> return pc_envs
                            (pc, env, _large) <- mergeEnvsPcs pc_envs'
                            pc' <- case pc.value of
                                ExprValueSMTExpr _ -> return pc
                                _ -> do
                                    name <- withEnv env $ addDef (pathCondName visitWithTag) pc
                                    return $ smtExprE boolT name
                            env' <- flip M.traverseWithKey env $ \(nm, typ) v -> do
                                case v of
                                    NotSplit v' | length (showSExprWithPlaceholders v') > 80 -> contractM nm visitWithTag.visit v' typ
                                    _ -> return v
                            return $ Just (pc', env')

getLoopPcEnvM :: MonadRepGraphE m => NodeAddr -> [Restr] -> m (Maybe (Expr, SMTEnv))
getLoopPcEnvM split vcount = do
    let vcount2 = flip withVCountMap vcount $ M.insert split (numberVC 0)
    prev_pc_envOpt <- getNodePcEnvM (Visit (Addr split) vcount2) Nothing
    case prev_pc_envOpt of
        Nothing -> return Nothing
        Just prev_pc_env -> do
            let (_, prev_env) = prev_pc_env
            mem_calls <- scanMemCalls prev_env >>= addLoopMemCallsM split
            let av nm typ = do
                    let nm2 = printf "%s_loop_at_%s" (nm :: String) (prettyNodeId (Addr split))
                    addVarR nm2 typ mem_calls
            (env, consts) <- flip runStateT S.empty $ flip M.traverseWithKey prev_env $ \(nm, typ) _v -> do
                let check_const = case typ of
                        ExprTypeHtd -> True
                        ExprTypeDom -> True
                        _ -> False
                isSyntConst <- lift $ if check_const then isSyntConstM nm typ split else return False
                if isSyntConst
                    then do
                        modify $ S.insert (nm, typ)
                        return $ prev_env ! (nm, typ)
                    else do
                        NotSplit . nameS <$> lift (av (nm.unwrap ++ "_after") typ)
            env' <- flip M.traverseWithKey env $ \(nm, typ) v -> do
                if S.member (nm, typ) consts
                    then return v
                    else do
                        z <- varRepRequest nm typ VarRepRequestKindLoop (Visit (Addr split) vcount) env
                        return $ fromMaybe v (Split <$> z)
            pc <- smtExprE boolT . NotSplit . nameS <$> av "pc_of" boolT
            return $ Just (pc, env')

getArcPcEnvsM :: MonadRepGraph m => NodeAddr -> Visit -> m [Maybe (Expr, SMTEnv)]
getArcPcEnvsM n n_vc2 = do
    r <- runExceptT $ do
        prevs <- prevsR n_vc2 <&> filter (\n_vc -> n_vc.nodeId == Addr n)
        ensureM $ length prevs <= 1
        for prevs $ \n_vc -> getArcPcEnvM n_vc n_vc2
    case r of
        Right x -> return x
        Left (TooGeneral { split }) -> do
            specs <- specializeM n_vc2 split
            let specs' =
                    [ Visit
                        { nodeId = n_vc2.nodeId
                        , restrs = spec
                        }
                    | spec <- specs
                    ]
            concat <$> (for specs' $ \spec -> getArcPcEnvsM n spec)

specializeM :: MonadRepGraph m => Visit -> NodeAddr -> m [[Restr]]
specializeM visit split = do
    let vcount = M.fromList $ [ (r.nodeAddr, r.visitCount) | r <- visit.restrs ]
    ensureM $ isOptionsVC $ vcount ! split
    for (enumerateSimpleVC (vcount ! split)) $ \n -> do
        return
            [ Restr
                { nodeAddr
                , visitCount
                }
            | (nodeAddr, visitCount) <- M.toAscList (M.insert split (fromSimpleVC n) vcount)
            ]

getArcPcEnvM :: MonadRepGraphE m => Visit -> Visit -> m (Maybe (Expr, SMTEnv))
getArcPcEnvM visit' n2 = do
    (_tag, vcountOpt) <- getTagVCount visit' Nothing
    case vcountOpt of
        Nothing -> return Nothing
        Just vcount -> do
            let visit = Visit
                    { nodeId = visit'.nodeId
                    , restrs = vcount
                    }
            present <- liftRepGraph $ use $ #arcPcEnvs % at visit
            case present of
                Just r -> return $ M.lookup n2.nodeId r
                Nothing -> do
                    getNodePcEnvM visit Nothing >>= \case
                        Nothing -> return Nothing
                        Just _ -> do
                            arcs <- emitNodeM visit
                            -- postEmitNodeHooksM visit
                            let arcs' = M.fromList [ (cont, (pc, env)) | (cont, pc, env) <- arcs ]
                            liftRepGraph $ #arcPcEnvs %= M.insert visit arcs'
                            return $ arcs' !? n2.nodeId

-- TODO
-- isContM :: MonadRepGraph m => Visit -> Visit

postEmitNodeHooksM :: MonadRepGraphE m => Visit -> m ()
postEmitNodeHooksM visit = do
    let n = visit.nodeId ^. expecting #_Addr
    tag <- nodeTagR n
    when (tag == C) $ do
        node <- liftRepGraph $ gview $ #problem % #nodes % at n % unwrapped
        let accs = toListOf (foldExprs % getMemAccess) node
        upd_ps <- catMaybes <$> (for accs $ \acc -> case acc.kind of
            MemOpKindUpdate -> Just <$> toSmtExprRM acc.addr visit Nothing
            _ -> return Nothing)
        case upd_ps of
            [] -> return ()
            _ -> error "unexpected"

addLocalDefMR :: MonadRepGraphE m => () -> () -> NameHint -> Expr -> ReaderT SMTEnv m MaybeSplit
addLocalDefMR _ _ = addDef

getFreshIdentMR :: MonadRepGraph m => NameHint -> m Ident
getFreshIdentMR hint = do
    problemNames <- liftRepGraph $ gview #problemNames
    extraProblemNames <- liftRepGraph $ use #extraProblemNames
    let taken n = S.member n problemNames || S.member n extraProblemNames
    let n = Ident $ chooseFreshName (taken . Ident) hint
    liftRepGraph $ #extraProblemNames %= S.insert n
    return $ n

emitNodeM :: MonadRepGraphE m => Visit -> m [(NodeId, Expr, SMTEnv)]
emitNodeM n = do
    (pc, env) <- fromJust <$> getNodePcEnvM n Nothing
    let nodeAddr = n.nodeId ^. expecting #_Addr
    tag <- nodeTagR nodeAddr
    let app_eqs = id
    node <- liftRepGraph $ gview $ #problem % #nodes % at nodeAddr % unwrapped
    if pc == falseE
        then return [ (c, falseE, M.empty) | c <- node ^.. nodeConts ]
        else case node of
            NodeCond condNode | condNode.left == condNode.right -> do
                return [(condNode.left, pc, env)]
            NodeCond condNode | condNode.expr == trueE -> do
                return [(condNode.left, pc, env), (condNode.right, falseE, env)]
            NodeBasic basicNode -> do
                upds <- for basicNode.varUpdates $ \upd -> do
                    let k = (upd.varName, upd.ty)
                    val <- case upd.expr.value of
                        ExprValueVar nm -> return $ env ! (nm, upd.expr.ty)
                        _ -> do
                            name <- return $ localName upd.varName n
                            let v = app_eqs upd.expr
                            vname <- withEnv env $ addLocalDefMR () () name v
                            return vname
                    return (k, val)
                let env' = M.union (M.fromList upds) env
                return [(basicNode.next, pc, env')]
            NodeCond condNode -> do
                let name = condName n
                freshName <- getFreshIdentMR name
                let cond = varE boolT freshName
                def <- withEnv env $ addLocalDefMR () () name (app_eqs condNode.expr)
                let env' = M.insert (freshName, boolT) def env
                let lpc = andE cond pc
                let rpc = andE (notE cond) pc
                return [(condNode.left, lpc, env'), (condNode.right, rpc, env')]
            NodeCall callNode -> do
                let nm = successName callNode.functionName n
                success' <- addVar nm boolT
                let success = smtExprE boolT $ NotSplit $ nameS success'
                sig <- liftRepGraph $ gview $ #functionSigs % to ($ (WithTag tag callNode.functionName))
                ins <- M.fromList <$> (for (zip sig.input callNode.input) $ \(funArg, callArg) -> do
                    x <- withEnv env $ smtExprM (app_eqs callArg)
                    return ((funArg.name, funArg.ty), x))
                mem_calls' <- scanMemCalls ins
                -- do
                --     for (M.toAscList ins) $ \((n, _), v) -> do
                --         case v of
                --             SMT s -> do
                --                 traceM $ "    %%% " ++ n.unwrap ++ " " ++ showSExprWithPlaceholders s
                --             _ -> return ()
                -- traceShowM $ ("mem_calls'", nodeCountName n, mem_calls')
                let mem_calls = addMemCall callNode.functionName $ mem_calls'
                -- traceShowM $ ("mem_calls", nodeCountName n, mem_calls)
                let m = do
                        for (zip callNode.output sig.output) $ \(Argument x typ, Argument y typ2) -> do
                            ensureM $ typ == typ2
                            let name = localName x n
                            var <- lift $ addVarR name typ mem_calls
                            modify $ M.insert (x, typ) (NotSplit $ nameS var)
                            tell [((y, typ2), (NotSplit $ nameS var))]
                        for (zip callNode.output sig.output) $ \(Argument x typ, Argument y _typ2) -> do
                            env' <- get
                            z <- lift $ varRepRequest x typ VarRepRequestKindCall n env'
                            case z of
                                Nothing -> return ()
                                Just z' -> do
                                    modify $ M.insert (x, typ) (Split z')
                                    tell [((y, typ), (Split z'))]
                (_, env', outs') <- runRWST m () env
                let outs = M.fromList outs'
                addFuncM callNode.functionName ins outs success n
                return $ [(callNode.next, pc, env')]

loopBodyInnerLoops :: Problem -> NodeAddr -> Set NodeAddr -> [Set NodeAddr]
loopBodyInnerLoops p hd loop_body = sccs
  where
    loop_set = S.delete hd loop_body
    (g, toNodeAddr', _) = G.graphFromEdges [((), n, filter (`S.member` loop_set) (p ^.. #nodes % at n % unwrapped % nodeConts % #_Addr)) | n <- S.toList loop_body]
    toNodeAddr = view _2 . toNodeAddr'
    sccs = do
        comp <- S.fromList . toList <$> G.scc g
        guard $ S.size comp > 1
        return $ S.map toNodeAddr comp

hasInnerLoopM :: MonadRepGraph m => NodeAddr -> m Bool
hasInnerLoopM hd = do
    present <- liftRepGraph $ use $ #hasInnerLoop % at hd
    case present of
        Just x -> return x
        Nothing -> do
            p <- liftRepGraph $ gview #problem
            body <- loopBodyR hd
            let x = not $ null $ loopBodyInnerLoops p hd body
            liftRepGraph $ #hasInnerLoop %= M.insert hd x
            return x

isSyntConstM :: forall m. MonadRepGraph m => Ident -> ExprType -> NodeAddr -> m Bool
isSyntConstM orig_nm typ split = do
    hasInnerLoop <- hasInnerLoopM split
    if hasInnerLoop
        then return False
        else do
            loop_set <- loopBodyR split
            let go :: Seq (Ident, NodeAddr) -> S.Set (Ident, NodeAddr) -> (Ident, NodeAddr) -> ExceptT Bool m Void
                go visit safe (nm, n) = do
                    let new_nm = nm
                    node <- liftRepGraph $ gview $ #problem % #nodes % at n % unwrapped
                    new_nm' <- case node of
                        NodeCall callNode ->
                            if Argument nm typ `elem` callNode.output
                            then throwError False
                            else return new_nm
                        NodeBasic basicNode -> do
                            let upds =
                                    [ vu.expr
                                    | vu <- basicNode.varUpdates
                                    , Argument vu.varName vu.ty == Argument nm typ
                                    ]
                            let vupds = for upds $ \v -> case v.value of
                                    ExprValueVar i -> Just i
                                    _ -> Nothing
                            case vupds of
                                Nothing -> throwError False
                                Just vupds' -> case vupds' of
                                    x:_ -> return x
                                    _ -> return new_nm
                        _ -> return new_nm
                    all_preds <- predsR $ Addr n
                    let preds = [ (new_nm', n2) | n2 <- S.toAscList all_preds, n2 `S.member` loop_set ]
                    let unknowns = [ p | p <- preds, p `S.notMember` safe ]
                    let (visit', safe') =
                            if null unknowns
                            then (visit, S.insert (nm, n) safe)
                            else (visit <> Seq.fromList [(nm, n)] <> Seq.fromList unknowns, safe)
                    let f :: Seq (Ident, NodeAddr) -> ExceptT Bool m Void
                        f v = case unsnoc v of
                            Nothing -> throwError True
                            Just (v', hd) -> do
                                if hd `S.member` safe'
                                    then f v'
                                    else if snd hd == split
                                        then throwError False
                                        else go v' safe' hd
                    f visit'

            runExceptT (go (mempty) (S.singleton (orig_nm, split)) (orig_nm, split)) >>= \case
                Left r -> return r

addMemCall :: Ident -> Maybe MemCalls -> Maybe MemCalls
addMemCall fname = fmap $ flip M.alter fname $ \slot -> Just $
    let f = (#min %~ (+1 )) . (#max % _Just %~ (+1 ))
     in fromMaybe zeroMemCallsForOne slot & f

getMemCalls :: MonadRepGraph m => SExprWithPlaceholders -> m MemCalls
getMemCalls mem_sexpr = do
    present <- liftRepGraph $ use $ #memCalls % at mem_sexpr
    case present of
        Just x -> do
            -- traceShowM ("present", x)
            return x
        Nothing -> do
            case mem_sexpr of
                List [op, x, _, _] | isStore op -> getMemCalls x
                List [op, _, x, y] | op == symbolS "ite" -> mergeMemCalls <$> getMemCalls x <*> getMemCalls y
                _ -> do
                    r <- runMaybeT $ do
                        name <- hoistMaybe $ parseSymbolS mem_sexpr
                        next <- MaybeT $ tryGetDef (Name name)
                        lift $ getMemCalls next
                    case r of
                        Just x -> return x
                        Nothing -> error $ "mem_calls fallthrough " ++ show (showSExprWithPlaceholders mem_sexpr)
  where
    isStore s = s `elem` ([symbolS "store-word32", symbolS "store-word8", symbolS "store-word64"] :: [SExprWithPlaceholders])

scanMemCalls :: MonadRepGraph m => SMTEnv -> m (Maybe MemCalls)
scanMemCalls env = do
    let mem_vs = [ v | ((_nm, typ), v) <- M.toAscList env, typ == memT ]
    mem_calls <- for (catMaybes (map (preview #_NotSplit) mem_vs)) $ \v -> do
        getMemCalls v
    -- traceShowM $ ("scanning", mem_calls)
    return $ case mem_calls of
        [] -> Nothing
        _ -> Just $ foldr1 mergeMemCalls mem_calls

addLoopMemCallsM :: MonadRepGraphE m => NodeAddr -> Maybe MemCalls -> m (Maybe MemCalls)
addLoopMemCallsM split mem_callsOpt = do
    case mem_callsOpt of
        Nothing -> return Nothing
        Just mem_calls -> do
            loopBody <- loopBodyR split
            fnames <- S.fromList . catMaybes <$> (for (S.toAscList loopBody) $ \n -> do
                node <- liftRepGraph $ gview $ #problem % #nodes % at n % unwrapped
                return $ case node of
                    NodeCall callNode -> Just callNode.functionName
                    _ -> Nothing)
            let new = M.fromList $ flip map (S.toList fnames) $ \fname -> (fname,) $
                    case M.lookup fname mem_calls of
                        Just x -> x & #max .~ Nothing
                        Nothing -> MemCallsForOne 0 Nothing
            if S.null fnames
            then return $ Just mem_calls
            -- else return $ Just $ M.unionWith f mem_calls $ M.fromList [ (fname, (MemCallsForOne 0 Nothing)) | fname <- S.toAscList fnames ]
            else return $ Just $ M.union new mem_calls

mergeMemCalls :: MemCalls -> MemCalls -> MemCalls
mergeMemCalls mem_calls_x mem_calls_y =
    if mem_calls_x == mem_calls_y
    then mem_calls_x
    else M.fromList $
        [ (k, f (fromMaybe zeroMemCallsForOne $ M.lookup k mem_calls_x) (fromMaybe zeroMemCallsForOne $ M.lookup k mem_calls_y))
        | k <- S.toList $ S.union (M.keysSet mem_calls_x) (M.keysSet mem_calls_y)
        ]
  where
    f x y = MemCallsForOne
        { min = min x.min y.min
        , max = liftA2 max x.max y.max
        }

getContM :: MonadRepGraph m => Visit -> m Visit
getContM visit = do
    conts <- liftRepGraph $ asks $ toListOf $ #problem % #nodes % at (visit.nodeId ^. expecting #_Addr) % unwrapped % nodeConts
    let p = case visit.nodeId of
            Addr addr | any (\restr -> restr.nodeAddr == addr) visit.restrs -> True
            _ -> False
    let [cont] = conts
    return $ Visit
        { nodeId = cont
        , restrs = if p then fromJust (incrVCs visit.restrs (visit.nodeId ^. expecting #_Addr) 1) else visit.restrs
        }

addFuncM :: MonadRepGraphE m => Ident -> SMTEnv -> SMTEnv -> Expr -> Visit -> m ()
addFuncM name inputs outputs success n_vc = do
    present <- liftRepGraph $ use $ #funcs % to (M.member n_vc)
    ensureM $ not present
    liftRepGraph $ #funcs %= M.insert n_vc (inputs, outputs, success)
    (liftRepGraph $ gview $ #pairingsAccess % at name) >>= \case
        Nothing -> return ()
        Just pair -> do
            -- traceShowM ("addFunc", name)
            group <- liftRepGraph $ use $ #funcsByName % to (fromMaybe [] . M.lookup pair)
            for_ group $ \n_vc2 -> do
                x <- getFuncPairingM n_vc n_vc2
                -- traceShowM ("ap", isJust x)
                when (isJust x) $ do
                    addFuncAssertM n_vc n_vc2
            liftRepGraph $ #funcsByName %= M.insert pair (group ++ [n_vc])

getFuncPairingNoCheckM :: MonadRepGraphE m => Visit -> Visit -> m (Maybe (Pairing, PairingOf Visit))
getFuncPairingNoCheckM n_vc n_vc2 = do
    n <- liftRepGraph $ gview $ #problem % #nodes % at (n_vc.nodeId ^. expecting #_Addr) % unwrapped % expecting #_NodeCall % #functionName
    n2 <- liftRepGraph $ gview $ #problem % #nodes % at (n_vc2.nodeId ^. expecting #_Addr) % unwrapped % expecting #_NodeCall % #functionName
    pair <- liftRepGraph $ gview $ #pairingsAccess % at n % unwrapped
    p <- liftRepGraph $ gview $ #pairings % #unwrap % at pair % unwrapped
    id $
        if PairingOf { asm = n, c = n2 } == pair
        then return (Just $ (p, PairingOf { asm = n_vc, c = n_vc2 }))
        else if PairingOf { asm = n2, c = n } == pair
        then return (Just $ (p, PairingOf { asm = n_vc2, c = n_vc }))
        else return (Nothing)

getFuncPairingM :: MonadRepGraphE m => Visit -> Visit -> m (Maybe (Pairing, PairingOf Visit))
getFuncPairingM n_vc n_vc2 = do
    getFuncPairingNoCheckM n_vc n_vc2 >>= \case
        Nothing -> return Nothing
        Just (p, p_n_vc) -> do
            (lin, _, _) <- liftRepGraph $ use $ #funcs % at p_n_vc.asm % unwrapped
            (rin, _, _) <- liftRepGraph $ use $ #funcs % at p_n_vc.c % unwrapped
            l_mem_calls <- scanMemCalls lin
            r_mem_calls <- scanMemCalls rin
            -- traceShowM ("calls", M.size <$> l_mem_calls, M.size <$> r_mem_calls)
            -- traceShowM ("callz", l_mem_calls, r_mem_calls)
            -- traceShowM ("callxl", (.unwrap) . fst <$> M.keys lin)
            -- traceShowM ("callxr", (.unwrap) . fst <$> M.keys rin)
            (c, _s) <- memCallsCompatible $ PairingOf
                { asm = l_mem_calls
                , c = r_mem_calls
                }
                -- { c = l_mem_calls
                -- , asm = r_mem_calls
                -- }
            unless c $ do
                -- traceShowM ("skipping", s)
                -- traceShowM "skip"
                return ()
            return $ if c then Just (p, p_n_vc) else Nothing

getFuncAssertM :: MonadRepGraphE m => Visit -> Visit -> m Expr
getFuncAssertM n_vc n_vc2 = do
    (pair, p_n_vc) <- fromJust <$> getFuncPairingM n_vc n_vc2
    (lin, lout, lsucc) <- liftRepGraph $ use $ #funcs % at p_n_vc.asm % unwrapped
    (rin, rout, rsucc) <- liftRepGraph $ use $ #funcs % at p_n_vc.c % unwrapped
    _lpc <- getPcM' p_n_vc.asm Nothing
    rpc <- getPcM' p_n_vc.c Nothing
    let envs = \case
            PairingEqSideQuadrant Asm PairingEqDirectionIn -> lin
            PairingEqSideQuadrant C PairingEqDirectionIn -> rin
            PairingEqSideQuadrant Asm PairingEqDirectionOut -> lout
            PairingEqSideQuadrant C PairingEqDirectionOut -> rout
    inp_eqs <- instEqsM pair.inEqs envs
    out_eqs <- instEqsM pair.outEqs envs
    let succ_imp = impliesE rsucc lsucc
    return $ impliesE (foldr1 andE (inp_eqs ++ [rpc])) (foldr1 andE (out_eqs ++ [succ_imp]))

instEqsM :: MonadSolver m => [PairingEq] -> (PairingEqSideQuadrant -> SMTEnv) -> m [Expr]
instEqsM eqs envs = for eqs $ \eq -> instEqWithEnvsM (eq.lhs.expr, envs eq.lhs.quadrant) (eq.rhs.expr, envs eq.rhs.quadrant)

addFuncAssertM :: MonadRepGraphE m => Visit -> Visit -> m ()
addFuncAssertM n_vc n_vc2 = do
    imp <- weakenAssert <$> getFuncAssertM n_vc n_vc2
    withoutEnv $ assertFact imp

memCallsCompatible :: MonadRepGraph m => PairingOf (Maybe MemCalls) -> m (Bool, Maybe String)
memCallsCompatible p_mem_calls = do
    case (p_mem_calls.asm, p_mem_calls.c) of
        (Just l_mem_calls, Just r_mem_calls) -> do
            r_cast_calls <- fmap M.fromList $ fmap catMaybes $ for (M.toAscList l_mem_calls) $ \(fname, calls) -> do
                pair <- liftRepGraph $ gview $ #pairingsAccess % at fname % unwrapped
                let r_fun = pair.c
                r_sig <- liftRepGraph $ gview $ #functionSigs % to ($ WithTag C r_fun)
                let memOut = any (\arg -> arg.ty == memT) r_sig.output
                -- traceShowM ("memOut", memOut)
                return $
                    if memOut
                    then Just (r_fun, calls)
                    else Nothing
            -- traceShowM ("r_cast_calls", r_cast_calls)
            -- traceShowM ("r_mem_calls", r_mem_calls)
            let f fname =
                    let r_cast = fromMaybe zeroMemCallsForOne $ r_cast_calls !? fname
                        r_actual = fromMaybe zeroMemCallsForOne $ r_mem_calls !? fname
                        x = case r_cast.max of
                                Just n -> n < r_actual.min
                                _ -> False
                        y = case r_actual.max of
                                Just n -> n < r_cast.min
                                _ -> False
                     in x || y
            let bad = any f (nub $ M.keys r_cast_calls ++ M.keys r_mem_calls)
            -- if bad
            --     then traceShowM ("not ok")
            --     else traceShowM ("okok")
            return $ if bad then (False, Just "foo") else (True, Nothing)
        _ -> return (True, Nothing)
