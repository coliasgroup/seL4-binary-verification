{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- {-# OPTIONS_GHC -Wno-name-shadowing #-}

module BV.Core.Stages.CompileProofChecks.RepGraph
    ( ArgRenames
    , FunctionSignature (..)
    , FunctionSignatures
    , MonadRepGraph (..)
    , RepGraphContext
    , RepGraphEnv
    , RepGraphState
    , getInductVarM
    , getNodePcEnvM
    , getNodePcEnvM'
    , getPcM
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
import BV.Core.Types

import BV.Core.Stages.Utils (chooseFreshName)
import BV.Core.Types.Extras (showSExprWithPlaceholders, uncheckedAtomS)
import BV.Core.Types.Extras.Expr
import BV.Core.Types.Extras.ProofCheck
import BV.Core.Utils
import Control.Applicative (asum)
import Control.DeepSeq (NFData)
import Control.Monad (filterM, guard, replicateM, unless, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (MonadReader)
import Control.Monad.RWS (MonadState (get, put), MonadWriter (..),
                          RWST (runRWST), evalRWST)
import Control.Monad.State (MonadState, StateT (runStateT), execStateT, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), hoistMaybe, runMaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Char (isAlpha)
import Data.Foldable (for_, toList)
import qualified Data.Graph as G
import Data.List (inits, intercalate, isPrefixOf, sort, tails)
import Data.List.Split (splitOn)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import Data.Vector.Internal.Check (HasCallStack)
import Data.Void (Void)
import Debug.Trace (traceM, traceShowId, traceShowM)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))
import Text.Printf (printf)

-- TODO cache more accross groups

type RepGraphContext m = (MonadReader RepGraphEnv m, MonadState RepGraphState m)

type ArgRenames = PairingEqSideQuadrant -> Ident -> Ident

class MonadSolver m => MonadRepGraph m where
    liftRepGraph :: (forall n. RepGraphContext n => n a) -> m a

instance MonadRepGraph m => MonadRepGraph (ExceptT e m) where
    liftRepGraph f = lift $ liftRepGraph f

data RepGraphEnv
  = RepGraphEnv
      { functionSigs :: FunctionSignatures
      , pairings :: Pairings
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
      , memCalls :: Map Name (Maybe MemCalls)
      , contractions :: Map SExprWithPlaceholders SMT
      , arcPcEnvs :: Map Visit (Map NodeId (Expr, SMTEnv))
      , extraProblemNames :: S.Set Ident
      , hasInnerLoop :: M.Map NodeAddr Bool
        --   , knownEqs :: Map VisitWithTag [KnownEqsValue]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

-- type KnownEqsValue = ()

data FunctionSignature
  = FunctionSignature
      { input :: [Argument]
      , output :: [Argument]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

type FunctionSignatures = WithTag Ident -> FunctionSignature

data LoopData
  = LoopHead (Set NodeAddr)
  | LoopMember NodeAddr
  deriving (Eq, Generic, Ord, Show)

initRepGraphEnv :: FunctionSignatures -> Pairings -> ArgRenames -> Problem -> RepGraphEnv
initRepGraphEnv functionSigs pairings argRenames problem =
    RepGraphEnv
        { functionSigs
        , pairings
        , argRenames
        , problem
        -- , nodeGraph = makeNodeGraph (map (_2 %~ view #node) (M.toAscList problem.nodes))
        , nodeGraph
        , nodeTag =
            let c = S.fromList . mapMaybe (preview #_Addr) $ reachableFrom nodeGraph problem.sides.c.entryPoint
             in \addr -> if addr `S.member` c then C else Asm
        , loopData =
            let heads = loopHeads nodeGraph [problem.sides.c.entryPoint, problem.sides.asm.entryPoint]
             in M.fromList $ flip foldMap heads $ \(loopHead, scc) ->
                    [(loopHead, LoopHead scc)] <> flip mapMaybe (S.toList scc) (\member ->
                        if member == loopHead then Nothing else Just (member, LoopMember loopHead))
        , preds =
            let defaults = map (, []) $ [Ret, Err] ++ map Addr (M.keys problem.nodes)
             in M.fromListWith (<>) $ concat $ [defaults] ++
                    [ [ (cont, S.singleton nodeAddr)
                    | cont <- node ^.. nodeConts
                    ]
                    | (nodeAddr, node) <- M.toAscList problem.nodes
                    ]
        , problemNames = S.fromList $ toListOf varNamesOf problem
        }
  where
    nodeGraph = makeNodeGraph (M.toAscList problem.nodes)

loopIdR :: MonadRepGraph m => NodeAddr -> m (Maybe NodeAddr)
loopIdR addr = liftRepGraph $ do
    loopData <- gview $ #loopData % at addr
    return (loopData <&> \case
        LoopHead _ -> addr
        LoopMember addr' -> addr')

loopIdR' :: MonadRepGraph m => NodeId -> m (Maybe NodeAddr)
loopIdR' = \case
    Addr addr -> loopIdR addr
    _ -> return Nothing

loopHeadsR :: MonadRepGraph m => m [NodeAddr]
loopHeadsR = liftRepGraph $ do
    loopData <- gview #loopData
    return (mapMaybe (\(k, v) -> case v of
        LoopHead _ -> Just k
        LoopMember _ -> Nothing) (M.toList loopData))

loopBodyR :: MonadRepGraph m => NodeAddr -> m (S.Set NodeAddr)
loopBodyR n = do
    head <- fromJust <$> loopIdR n
    loopData <- liftRepGraph $ gview $ #loopData % at head % unwrapped
    let LoopHead body = loopData
    return body

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
        x <- lift $ addVarMR (arg.name.unwrap ++ "_init") arg.ty (Just M.empty)
        modify $ M.insert (arg.name, arg.ty) (SMT $ nameS x)
    for_ args $ \arg -> do
        env <- get
        z <- lift $ varRepRequest arg.name arg.ty VarRepRequestKindInit (Visit { nodeId = n, restrs = []}) env
        case z of
            Nothing -> return ()
            Just z' -> do
                modify $ M.insert (arg.name, arg.ty) (SMTSplitMem z')

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
            vname <- addVarM (printf "induct_i_%d_%d" induct.a induct.b) word32T
            liftRepGraph $ #inductVarEnv %= M.insert induct (vname)
            return vname
    return $ smtExprE word32T (SMT $ nameS vname)

substInduct :: Expr -> Expr -> Expr
substInduct expr inductVar = flip varSubstNotMust expr $ \ident ty ->
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

addVarMR :: MonadRepGraph m => NameHint -> ExprType -> Maybe MemCalls -> m Name
addVarMR nameHint ty memCallsOpt = do
    r <- addVarRestrM nameHint ty
    when (ty == ExprTypeMem) $ do
        liftRepGraph $ #memCalls %= M.insert r memCallsOpt
    return r

varRepRequest :: MonadRepGraph m => Ident -> ExprType -> VarRepRequestKind -> Visit -> SMTEnv -> m (Maybe SplitMem)
varRepRequest nm typ kind n_vc env = runMaybeT $ do
    let hook = asmStackRepHook
    let n = n_vc.nodeId ^. expecting #_Addr
    addr <- MaybeT $ hook nm typ kind n
    addr_s <- lift $ withEnv env $ smtExprM addr
    let countName = nodeCountName n_vc
    let name = printf "%s_for_%s" nm.unwrap countName
    lift $ addSplitMemVarM (addr_s ^. expecting #_SMT) name typ

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

contractM :: MonadRepGraph m => Ident -> Visit -> SExprWithPlaceholders -> ExprType -> m SMT
contractM name n_vc val typ = do
    liftRepGraph (use $ #contractions % at val) >>= \case
        Just x -> return x
        Nothing -> do
            let name' = localNameBefore name n_vc
            name'' <- withoutEnv $ addDefM name' (smtExprE typ (SMT val))
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
                                    name <- withEnv env $ addDefM (pathCondName visitWithTag) pc
                                    return $ smtExprE boolT name
                            env' <- flip M.traverseWithKey env $ \(nm, typ) v -> do
                                case v of
                                    SMT v' | length (showSExprWithPlaceholders v') > 80 -> contractM nm visitWithTag.visit v' typ
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
                    addVarMR nm2 typ mem_calls
            (env, consts) <- flip runStateT S.empty $ flip M.traverseWithKey prev_env $ \(nm, typ) v -> do
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
                        SMT . nameS <$> lift (av (nm.unwrap ++ "_after") typ)
            env' <- flip M.traverseWithKey env $ \(nm, typ) v -> do
                if S.member (nm, typ) consts
                    then return v
                    else do
                        z <- varRepRequest nm typ VarRepRequestKindLoop (Visit (Addr split) vcount) env
                        return $ fromMaybe v (SMTSplitMem <$> z)
            pc <- smtExprE boolT . SMT . nameS <$> av "pc_of" boolT
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
            | (nodeAddr, visitCount) <- M.toAscList (M.insert split (fromSimpleVisitCountView n) vcount)
            ]

getArcPcEnvM :: MonadRepGraphE m => Visit -> Visit -> m (Maybe (Expr, SMTEnv))
getArcPcEnvM visit' n2 = do
    (tag, vcountOpt) <- getTagVCount visit' Nothing
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

addLocalDefMR :: MonadRepGraphE m => () -> () -> NameHint -> Expr -> ReaderT SMTEnv m SMT
addLocalDefMR _ _ = addDefM

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
                condName <- getFreshIdentMR name
                let cond = varE boolT condName
                def <- withEnv env $ addLocalDefMR () () name (app_eqs condNode.expr)
                let env' = M.insert (condName, boolT) def env
                let lpc = andE cond pc
                let rpc = andE (notE cond) pc
                return [(condNode.left, lpc, env'), (condNode.right, rpc, env')]
            NodeCall callNode -> do
                let nm = successName callNode.functionName n
                success' <- addVarM nm boolT
                let success = smtExprE boolT $ SMT $ nameS success'
                sig <- liftRepGraph $ gview $ #functionSigs % to ($ (WithTag tag callNode.functionName))
                ins <- M.fromList <$> (for (zip sig.input callNode.input) $ \(funArg, callArg) -> do
                    x <- withEnv env $ smtExprM (app_eqs callArg)
                    return ((funArg.name, funArg.ty), x))
                mem_calls <- addMemCall callNode.functionName <$> scanMemCalls ins
                let m = do
                        for (zip callNode.output sig.output) $ \(Argument x typ, Argument y typ2) -> do
                            ensureM $ typ == typ2
                            let name = localName x n
                            var <- lift $ addVarMR name typ mem_calls
                            modify $ M.insert (x, typ) (SMT $ nameS var)
                            tell [((y, typ2), (SMT $ nameS var))]
                        for (zip callNode.output sig.output) $ \(Argument x typ, Argument y typ2) -> do
                            env' <- get
                            z <- lift $ varRepRequest x typ VarRepRequestKindCall n env'
                            case z of
                                Nothing -> return ()
                                Just z' -> do
                                    modify $ M.insert (x, typ) (SMTSplitMem z')
                                    tell [((y, typ), (SMTSplitMem z'))]
                (_, env', outs') <- runRWST m () env
                let outs = M.fromList outs'
                addFuncM callNode.functionName ins outs success n
                return $ [(callNode.next, pc, env')]

loopBodyInnerLoops :: Problem -> NodeAddr -> Set NodeAddr -> [Set NodeAddr]
loopBodyInnerLoops p head loop_body = sccs
  where
    loop_set = S.delete head loop_body
    (g, toNodeAddr', _) = G.graphFromEdges [((), n, filter (`S.member` loop_set) (p ^.. #nodes % at n % unwrapped % nodeConts % #_Addr)) | n <- S.toList loop_body]
    toNodeAddr = view _2 . toNodeAddr'
    sccs = do
        comp <- S.fromList . toList <$> G.scc g
        guard $ S.size comp > 1
        return $ S.map toNodeAddr comp

hasInnerLoopM :: MonadRepGraph m => NodeAddr -> m Bool
hasInnerLoopM head = do
    present <- liftRepGraph $ use $ #hasInnerLoop % at head
    case present of
        Just x -> return x
        Nothing -> do
            p <- liftRepGraph $ gview #problem
            body <- loopBodyR head
            let x = not $ null $ loopBodyInnerLoops p head body
            liftRepGraph $ #hasInnerLoop %= M.insert head x
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

addFuncM :: MonadRepGraphE m => Ident -> SMTEnv -> SMTEnv -> Expr -> Visit -> m ()
addFuncM name inputs outputs success n_vc = do
    -- TODO
    return ()

addMemCall :: Ident -> Maybe MemCalls -> Maybe MemCalls
addMemCall fname = fmap $ flip M.alter fname $ \slot -> Just $
    let f = (#min %~ (+1 )) . (#max % _Just %~ (+1 ))
     in fromMaybe zeroMemCallsForOne slot & f

getMemCalls :: MonadRepGraph m => SExprWithPlaceholders -> m MemCalls
getMemCalls mem_sexpr = do
    undefined

scanMemCalls :: MonadRepGraph m => SMTEnv -> m (Maybe MemCalls)
scanMemCalls env = do
    let mem_vs = [ v | ((_nm, typ), v) <- M.toAscList env, typ == memT ]
    mem_calls <- for (catMaybes (map (preview #_SMT) mem_vs)) $ \v -> do
        getMemCalls v
    return $ case mem_calls of
        [] -> Nothing
        _ -> Just $ foldr1 mergeMemCalls mem_calls

addLoopMemCallsM :: MonadRepGraphE m => NodeAddr -> Maybe MemCalls -> m (Maybe MemCalls)
addLoopMemCallsM split mem_calls = do
    -- TODO
    return Nothing

mergeMemCalls :: MemCalls -> MemCalls -> MemCalls
mergeMemCalls mem_calls_x mem_calls_y =
    if mem_calls_x == mem_calls_y
    then mem_calls_x
    else M.unionWith f mem_calls_x mem_calls_y
  where
    f x y = MemCallsForOne
        { min = min x.min y.min
        , max = liftA2 max x.max y.max
        }
