{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module BV.Core.Stages.CompileProofChecks.RepGraph
    ( FunctionSignatures
    , MonadRepGraph (..)
    , RepGraphEnv
    , RepGraphState
    , getInductVar
    , getNodePcEnv
    , getPc
    , initRepGraph
    , initRepGraphEnv
    , initRepGraphState
    , instEqWithEnvs
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

import Control.DeepSeq (NFData)
import Control.Monad (filterM, guard, replicateM, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (Reader, ReaderT)
import Control.Monad.RWS (MonadState (get, put), MonadWriter (..),
                          RWST (runRWST), evalRWST)
import Control.Monad.State (StateT (runStateT), execStateT, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), hoistMaybe, runMaybeT)
import Data.Char (isAlpha)
import Data.Foldable (for_, toList)
import qualified Data.Graph as G
import Data.List (intercalate, isPrefixOf, sort, tails)
import Data.List.Split (splitOn)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))
import Text.Printf (printf)

-- TODO cache more accross groups?

type FunctionSignatures = WithTag Ident -> FunctionSignature

class MonadSolver m => MonadRepGraph m where
    liftRepGraph :: StateT RepGraphState (Reader RepGraphEnv) a -> m a

    runPreEmitNodeHook :: Visit -> m ()
    runPreEmitNodeHook _ = return ()

instance MonadRepGraph m => MonadRepGraph (ReaderT r m) where
    liftRepGraph = lift . liftRepGraph
    runPreEmitNodeHook = lift . runPreEmitNodeHook

instance MonadRepGraph m => MonadRepGraph (StateT s m) where
    liftRepGraph = lift . liftRepGraph
    runPreEmitNodeHook = lift . runPreEmitNodeHook

instance (Monoid w, MonadRepGraph m) => MonadRepGraph (RWST r w s m) where
    liftRepGraph = lift . liftRepGraph
    runPreEmitNodeHook = lift . runPreEmitNodeHook

instance MonadRepGraph m => MonadRepGraph (MaybeT m) where
    liftRepGraph = lift . liftRepGraph
    runPreEmitNodeHook = lift . runPreEmitNodeHook

instance MonadRepGraph m => MonadRepGraph (ExceptT e m) where
    liftRepGraph = lift . liftRepGraph
    runPreEmitNodeHook = lift . runPreEmitNodeHook

type MonadRepGraphE m = (MonadRepGraph m, MonadError TooGeneral m)

data TooGeneral
  = TooGeneral
      { split :: NodeAddr
      }
  deriving (Eq, Generic, Ord, Show)

data RepGraphEnv
  = RepGraphEnv
      { functionSigs :: FunctionSignatures
      , pairings :: Pairings
      , pairingsAccess :: M.Map Ident PairingId
      , problem :: Problem
      , argRenames :: ArgRenames
      , problemNames :: S.Set Ident
      , nodeGraph :: NodeGraph
      , nodeTag :: NodeAddr -> Tag
      , loopData :: Map NodeAddr LoopData
      , preds :: Map NodeId (Set NodeAddr)
      }
  deriving (Generic)

data RepGraphState
  = RepGraphState
      { inpEnvs :: Map NodeId ExprEnv
      , memCalls :: Map SExprWithPlaceholders MemCalls
      , nodePcEnvs :: Map VisitWithTag (Maybe (Expr, ExprEnv))
      , arcPcEnvs :: Map Visit (Map NodeId (Expr, ExprEnv))
      , funcs :: M.Map Visit (ExprEnv, ExprEnv, Expr)
      , funcsByName :: M.Map (PairingOf Ident) [Visit]
      , inductVarEnv :: Map EqHypInduct Name
      , contractions :: Map SExprWithPlaceholders MaybeSplit
      , extraProblemNames :: S.Set Ident
      , hasInnerLoop :: M.Map NodeAddr Bool
      }
  deriving (Eq, Generic, NFData, Ord, Show)

initRepGraphEnv :: FunctionSignatures -> Pairings -> ArgRenames -> Problem -> RepGraphEnv
initRepGraphEnv functionSigs pairings argRenames problem =
    RepGraphEnv
        { functionSigs
        , pairings
        , pairingsAccess = M.fromListWith (error "unexpected") $
            concat [ [(p.c, p), (p.asm, p)] | p <- M.keys pairings.unwrap]
        , problem
        , argRenames
        , problemNames = S.fromList $ toListOf varNamesOfProblem problem
        , nodeGraph
        , nodeTag = nodeTagOf problem nodeGraph
        , loopData = createLoopDataMap problem nodeGraph
        , preds = predsOf problem
        }
  where
    nodeGraph = makeNodeGraph (M.toAscList problem.nodes)

initRepGraphState :: RepGraphState
initRepGraphState = RepGraphState
    { inpEnvs = M.empty
    , memCalls = M.empty
    , nodePcEnvs = M.empty
    , arcPcEnvs = M.empty
    , funcs = M.empty
    , funcsByName = M.empty
    , inductVarEnv = M.empty
    , contractions = M.empty
    , extraProblemNames = S.empty
    , hasInnerLoop = M.empty
    }

initRepGraph :: MonadRepGraph m => m ()
initRepGraph = do
    addInputEnvs

--

withMapSlot :: (MonadRepGraph m, Ord k) => Lens' RepGraphState (M.Map k v) -> k -> m v -> m v
withMapSlot l k m = do
    opt <- liftRepGraph (use (l % at k))
    whenNothing opt $ do
        v <- m
        liftRepGraph $ l %= M.insertWith (error "unexpected") k v
        return v

--

askNodeTag :: MonadRepGraph m => NodeAddr -> m Tag
askNodeTag n = liftRepGraph $ gview $ #nodeTag % to ($ n)

askIsNonTriviallyReachableFrom :: MonadRepGraph m => NodeAddr -> NodeId -> m Bool
askIsNonTriviallyReachableFrom from to_ = do
    g <- liftRepGraph $ gview #nodeGraph
    fromNode <- liftRepGraph $ gview $ #problem % #nodes % at from % unwrapped
    return $ or [ isReachableFrom g fromCont to_ | fromCont <- fromNode ^.. nodeConts ]

askLoopHead :: MonadRepGraph m => NodeAddr -> m (Maybe NodeAddr)
askLoopHead addr = liftRepGraph $ loopHeadOf addr <$> gview #loopData

askLoopBody :: MonadRepGraph m => NodeAddr -> m (S.Set NodeAddr)
askLoopBody n = liftRepGraph $ loopBodyOf n <$> gview #loopData

askPreds :: MonadRepGraph m => NodeId -> m (Set NodeAddr)
askPreds n = liftRepGraph $ gview $ #preds % at n % unwrapped

askPrevs :: MonadRepGraph m => Visit -> m [Visit]
askPrevs visit = do
    let m = toMapVC visit.restrs
    preds <- S.toAscList <$> askPreds visit.nodeId
    let f p = Visit (Addr p) <$>
            if M.member p m
            then incrVCs visit.restrs p (-1)
            else Just visit.restrs
    return $ mapMaybe f preds

incrVCs :: [Restr] -> NodeAddr -> Integer -> Maybe [Restr]
incrVCs vcount n incr =
    if isEmptyVC vc
    then Nothing
    else Just (fromMapVC (M.insert n vc m))
  where
    m = toMapVC vcount
    vc = incrVC incr (m ! n)

getHasInnerLoop :: MonadRepGraph m => NodeAddr -> m Bool
getHasInnerLoop loopHead = withMapSlot #hasInnerLoop loopHead $ do
    p <- liftRepGraph $ gview #problem
    loopBody <- askLoopBody loopHead
    return $ not $ null $ loopBodyInnerLoops p loopHead loopBody

loopBodyInnerLoops :: Problem -> NodeAddr -> Set NodeAddr -> [Set NodeAddr]
loopBodyInnerLoops p loopHead loopBody =
    [ S.map (view _2 . toNodeAddr) component
    | component <- S.fromList . toList <$> G.scc g
    , S.size component > 1
    ]
  where
    loopSet = S.delete loopHead loopBody
    addrConts n = p ^.. #nodes % at n % unwrapped % nodeConts % #_Addr
    (g, toNodeAddr, _) = G.graphFromEdges
        [ ( ()
          , n
          , filter (`S.member` loopSet) (addrConts n)
          )
        | n <- S.toList loopBody
        ]

getFreshIdent :: MonadRepGraph m => NameHint -> m Ident
getFreshIdent nameHint = do
    problemNames <- liftRepGraph $ gview #problemNames
    extraProblemNames <- liftRepGraph $ use #extraProblemNames
    let taken n = S.member n problemNames || S.member n extraProblemNames
    let n = Ident $ chooseFreshName (taken . Ident) nameHint
    liftRepGraph $ #extraProblemNames %= S.insert n
    return n

--

localNameBefore :: Ident -> Visit -> NameHint
localNameBefore s vis = printf "%s_v_at_%s" s.unwrap (nodeCountName vis)

localName :: Ident -> Visit -> NameHint
localName s vis = printf "%s_after_%s" s.unwrap (nodeCountName vis)

condName :: Visit -> NameHint
condName  vis = printf "cond_at_%s" (nodeCountName vis)

pathCondName :: VisitWithTag -> NameHint
pathCondName vt = printf "path_cond_to_%s_%s" (nodeCountName vt.visit) (prettyTag vt.tag)

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

getMemCalls :: MonadRepGraph m => SExprWithPlaceholders -> m MemCalls
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

scanMemCalls :: MonadRepGraph m => ExprEnv -> m (Maybe MemCalls)
scanMemCalls env = do
    let vals = [ v | ((_, ty), v) <- M.toAscList env, ty == memT ]
    memCalls <- traverse getMemCalls (vals ^.. folded % #_NotSplit)
    return $ case memCalls of
        [] -> Nothing
        _ -> Just $ foldr1 mergeMemCalls memCalls

addLoopMemCalls :: MonadRepGraphE m => NodeAddr -> Maybe MemCalls -> m (Maybe MemCalls)
addLoopMemCalls split = traverse $ \memCalls -> do
    loopBody <- askLoopBody split
    fnames <- fmap (S.fromList . catMaybes) $ for (S.toAscList loopBody) $ \n -> do
        node <- liftRepGraph $ gview $ #problem % #nodes % at n % unwrapped
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

memCallsCompatible :: MonadRepGraph m => PairingOf (Maybe MemCalls) -> m (Bool, Maybe String)
memCallsCompatible = \case
    PairingOf { asm = Just lcalls, c = Just rcalls } -> do
        rcastcalls <- fmap (M.fromList . catMaybes) $ for (M.toAscList lcalls) $ \(fname, calls) -> do
            pairingId <- liftRepGraph $ gview $ #pairingsAccess % at fname % unwrapped
            let rfname = pairingId.c
            rsig <- liftRepGraph $ gview $ #functionSigs % to ($ WithTag C rfname)
            return $
                if any (\arg -> arg.ty == memT) rsig.output
                then Just (rfname, calls)
                else Nothing
        let isIncompat fname =
                let rcast = fromMaybe zeroMemCallsForFunction $ rcastcalls !? fname
                    ractual = fromMaybe zeroMemCallsForFunction $ rcalls !? fname
                    x = case rcast.max of
                            Just n -> n < ractual.min
                            _ -> False
                    y = case ractual.max of
                            Just n -> n < rcast.min
                            _ -> False
                    in x || y
        let incompat = any isIncompat $ S.toList $ M.keysSet rcastcalls <> M.keysSet rcalls
        return $ if incompat then (False, error "unimplemented") else (True, Nothing)
    _ -> return (True, Nothing)

--

-- TODO
addLocalDef :: MonadRepGraphE m => () -> () -> NameHint -> Expr -> ReaderT ExprEnv m MaybeSplit
addLocalDef _ _ = addDef

addVarRestrWithMemCalls :: MonadRepGraph m => NameHint -> ExprType -> Maybe MemCalls -> m Name
addVarRestrWithMemCalls nameHint ty memCallsOpt = do
    r <- addVarRestr nameHint ty
    when (isMemT ty) $ do
        liftRepGraph $ #memCalls %= M.insert (nameS r) (fromJust memCallsOpt)
    return r

--

contract :: MonadRepGraph m => Ident -> Visit -> SExprWithPlaceholders -> ExprType -> m MaybeSplit
contract name visit sexpr ty = withMapSlot #contractions sexpr $ do
    let name' = localNameBefore name visit
    withoutEnv $ addDef name' (smtExprE ty (NotSplit sexpr))

specialize :: Visit -> NodeAddr -> [[Restr]]
specialize visit split = ensure (isOptionsVC vc)
    [ fromMapVC $ M.insert split (fromSimpleVC n) m
    | n <- enumerateSimpleVC vc
    ]
  where
    m = toMapVC visit.restrs
    vc = m ! split

--

addFunc :: MonadRepGraphE m => Ident -> ExprEnv -> ExprEnv -> Expr -> Visit -> m ()
addFunc name inputs outputs success visit = do
    liftRepGraph $ #funcs %= M.insertWith (error "unexpected") visit (inputs, outputs, success)
    pairingIdOpt <- liftRepGraph $ gview $ #pairingsAccess % at name
    whenJust_ pairingIdOpt $ \pairingId -> do
        group <- liftRepGraph $ use $ #funcsByName % to (fromMaybe [] . M.lookup pairingId)
        for_ group $ \visit2 -> do
            ok <- isJust <$> getFuncPairing visit visit2
            when ok $ do
                addFuncAssert visit visit2
        liftRepGraph $ #funcsByName %= M.insert pairingId (group ++ [visit])

getFuncPairingNoCheck :: MonadRepGraphE m => Visit -> Visit -> m (Maybe (Pairing, PairingOf Visit))
getFuncPairingNoCheck visit visit2 = do
    let askFnName v = liftRepGraph $ gview $
            #problem % #nodes % at (nodeAddrFromNodeId v.nodeId) % unwrapped % expecting #_NodeCall % #functionName
    fname <- askFnName visit
    fname2 <- askFnName visit2
    pairingId <- liftRepGraph $ gview $ #pairingsAccess % at fname % unwrapped
    p <- liftRepGraph $ gview $ #pairings % #unwrap % at pairingId % unwrapped
    return $ (p ,) <$> if
        | pairingId == PairingOf { asm = fname, c = fname2 } -> Just $ PairingOf { asm = visit, c = visit2 }
        | pairingId == PairingOf { asm = fname2, c = fname } -> Just $ PairingOf { asm = visit2, c = visit }
        | otherwise -> Nothing

getFuncPairing :: MonadRepGraphE m => Visit -> Visit -> m (Maybe (Pairing, PairingOf Visit))
getFuncPairing visit visit2 = do
    opt <- getFuncPairingNoCheck visit visit2
    whenJustThen opt $ \(p, visits) -> do
        (lin, _, _) <- liftRepGraph $ use $ #funcs % at visits.asm % unwrapped
        (rin, _, _) <- liftRepGraph $ use $ #funcs % at visits.c % unwrapped
        lcalls <- scanMemCalls lin
        rcalls <- scanMemCalls rin
        (compatible, _s) <- memCallsCompatible $ PairingOf
            { asm = lcalls
            , c = rcalls
            }
        -- unless compatible $ do
        --     warn _s
        return $ if compatible then Just (p, visits) else Nothing

getFuncAssert :: MonadRepGraphE m => Visit -> Visit -> m Expr
getFuncAssert visit visit2 = do
    (pairing, visits) <- fromJust <$> getFuncPairing visit visit2
    (lin, lout, lsucc) <- liftRepGraph $ use $ #funcs % at visits.asm % unwrapped
    (rin, rout, rsucc) <- liftRepGraph $ use $ #funcs % at visits.c % unwrapped
    _lpc <- getPc visits.asm Nothing
    rpc <- getPc visits.c Nothing
    let envs = \case
            PairingEqSideQuadrant Asm PairingEqDirectionIn -> lin
            PairingEqSideQuadrant C PairingEqDirectionIn -> rin
            PairingEqSideQuadrant Asm PairingEqDirectionOut -> lout
            PairingEqSideQuadrant C PairingEqDirectionOut -> rout
    inEqs <- instEqs pairing.inEqs envs
    outEqs <- instEqs pairing.outEqs envs
    let succImp = impliesE rsucc lsucc
    return $ impliesE
        (foldr1 andE (inEqs ++ [rpc]))
        (foldr1 andE (outEqs ++ [succImp]))
  where
    instEqs :: MonadSolver m => [PairingEq] -> (PairingEqSideQuadrant -> ExprEnv) -> m [Expr]
    instEqs eqs envs = for eqs $ \eq ->
        instEqWithEnvs (eq.lhs.expr, envs eq.lhs.quadrant) (eq.rhs.expr, envs eq.rhs.quadrant)

addFuncAssert :: MonadRepGraphE m => Visit -> Visit -> m ()
addFuncAssert visit visit2 = do
    imp <- weakenAssert <$> getFuncAssert visit visit2
    withoutEnv $ assertFact imp

--

data VarRepRequestKind
  = VarRepRequestKindCall
  | VarRepRequestKindInit
  | VarRepRequestKindLoop
  deriving (Eq, Generic, Ord, Show)

varRepRequest :: MonadRepGraph m => Ident -> ExprType -> VarRepRequestKind -> Visit -> ExprEnv -> m (Maybe SplitMem)
varRepRequest name ty kind visit env = runMaybeT $ do
    let hook = asmStackRepHook
    let n = nodeAddrFromNodeId visit.nodeId
    addrExpr <- MaybeT $ hook name ty kind n
    addrSexpr <- withEnv env $ convertExpr addrExpr
    let name' = printf "%s_for_%s" name.unwrap (nodeCountName visit)
    addSplitMemVar (addrSexpr ^. expecting #_NotSplit) name' ty

-- TOD rename
asmStackRepHook :: MonadRepGraph m => Ident -> ExprType -> VarRepRequestKind -> NodeAddr -> m (Maybe Expr)
asmStackRepHook name ty kind n = runMaybeT $ do
    tag <- askNodeTag n
    guard $ tag == Asm
    guard $ "stack" `isPrefixOf` name.unwrap
    guard $ ty == ExprTypeMem
    guard $ kind /= VarRepRequestKindInit
    argRenames <- liftRepGraph $ gview #argRenames
    return $ varE word32T $ argRenames
        (PairingEqSideQuadrant
            { tag
            , direction = PairingEqDirectionIn
            })
        (Ident "r13")

--

addInputEnvs :: MonadRepGraph m => m ()
addInputEnvs = do
    p <- liftRepGraph $ gview #problem
    f p.sides.asm
    f p.sides.c
  where
    f side = do
        let m = do
                for_ side.input $ \arg -> do
                    var <- addVarRestrWithMemCalls
                        (arg.name.unwrap ++ "_init")
                        arg.ty
                        (Just M.empty)
                    modify $ M.insert (arg.name, arg.ty) (NotSplit (nameS var))
                for_ side.input $ \arg -> do
                    env <- get
                    opt <- varRepRequest
                        arg.name
                        arg.ty
                        VarRepRequestKindInit
                        (Visit { nodeId = side.entryPoint, restrs = []})
                        env
                    whenJust_ opt $ \splitMem -> modify $ M.insert (arg.name, arg.ty) (Split splitMem)
        env <- execStateT m M.empty
        liftRepGraph $ #inpEnvs %= M.insert side.entryPoint env

getPc :: MonadRepGraph m => Visit -> Maybe Tag -> m Expr
getPc visit tag = view (expecting _Right) <$> runExceptT (tryGetPc visit tag)

tryGetPc :: MonadRepGraphE m => Visit -> Maybe Tag -> m Expr
tryGetPc visit tag = tryGetNodePcEnv visit tag >>= \case
    Nothing -> return falseE
    Just (pc, env) -> withEnv env $ convertInnerExpr pc

getInductVar :: MonadRepGraph m => EqHypInduct -> m Expr
getInductVar induct =
    fmap (smtExprE word32T . NotSplit . nameS) $
        withMapSlot #inductVarEnv induct $
            addVar (printf "induct_i_%d_%d" induct.a induct.b) word32T

substInduct :: Expr -> Expr -> Expr
substInduct expr inductVar = varSubst f expr
  where
    f (Ident "%n") (ExprTypeWord 32) = Just inductVar
    f _ _ = Nothing

instEqWithEnvs :: MonadSolver m => (Expr, ExprEnv) -> (Expr, ExprEnv) -> m Expr
instEqWithEnvs (x, xenv) (y, yenv) = do
    x' <- withEnv xenv $ convertUnderOp x
    y' <- withEnv yenv $ convertUnderOp y
    let f = case x'.ty of
            ExprTypeRelWrapper -> applyRelWrapper
            _ -> eqE
    return $ f x' y'
  where
    convertUnderOp :: MonadSolver m => Expr -> ReaderT ExprEnv m Expr
    convertUnderOp expr = case expr.value of
        ExprValueOp op args -> do
            args' <- traverse convertInnerExpr args
            return $ Expr expr.ty $ ExprValueOp op args'
        _ -> convertInnerExpr expr

--

getNodePcEnv :: MonadRepGraph m => Visit -> Maybe Tag -> m (Maybe (Expr, ExprEnv))
getNodePcEnv visit tag = view (expecting _Right) <$> runExceptT (tryGetNodePcEnv visit tag)

tryGetNodePcEnv :: MonadRepGraphE m => Visit -> Maybe Tag -> m (Maybe (Expr, ExprEnv))
tryGetNodePcEnv visit tag' = do
    (tag, restrsOpt) <- getTagVCount visit tag'
    runMaybeT $ do
        restrs <- hoistMaybe restrsOpt
        let vt = VisitWithTag
                { visit = Visit
                    { nodeId = visit.nodeId
                    , restrs
                    }
                , tag
                }
        MaybeT $ withMapSlot #nodePcEnvs vt $ do
            warmPcEnvCache vt
            getNodePcEnvRaw vt

getNodePcEnvRaw :: MonadRepGraphE m => VisitWithTag -> m (Maybe (Expr, ExprEnv))
getNodePcEnvRaw visitWithTag = do
    liftRepGraph (use $ #inpEnvs % at visitWithTag.visit.nodeId) >>= \case
        Just env -> return $ Just (trueE, env)
        Nothing -> do
            let f restr = Addr restr.nodeAddr == visitWithTag.visit.nodeId && restr.visitCount == offsetVC 0
            case filter f visitWithTag.visit.restrs of
                restr:_ -> getLoopPcEnv restr.nodeAddr visitWithTag.visit.restrs
                [] -> do
                    pcEnvs <- concatMap catMaybes <$> do
                        preds <- askPreds visitWithTag.visit.nodeId
                        for (S.toList preds) $ \pred_ -> do
                            tag <- askNodeTag pred_
                            if tag /= visitWithTag.tag
                                then return []
                                else getArcPcEnvs pred_ visitWithTag.visit
                    case pcEnvs of
                        [] -> return Nothing
                        _ -> do
                            pcEnvs' <- case visitWithTag.visit.nodeId of
                                Err -> for pcEnvs $ \(pc, env) -> do
                                    pc' <- withEnv env $ convertInnerExpr pc
                                    return (pc', M.empty)
                                _ -> return pcEnvs
                            (pc, env, _large) <- mergeEnvsPcs pcEnvs'
                            pc' <- case pc.value of
                                ExprValueSMTExpr _ -> return pc
                                _ -> do
                                    name <- withEnv env $ addDef (pathCondName visitWithTag) pc
                                    return $ smtExprE boolT name
                            env' <- flip M.traverseWithKey env $ \(name, ty) v -> do
                                case v of
                                    NotSplit v' | length (showSExprWithPlaceholders v') > 80 -> contract name visitWithTag.visit v' ty
                                    _ -> return v
                            return $ Just (pc', env')

getLoopPcEnv :: MonadRepGraphE m => NodeAddr -> [Restr] -> m (Maybe (Expr, ExprEnv))
getLoopPcEnv split restrs = do
    let restrs2 = withMapVC (M.insert split (numberVC 0)) restrs
    prevPcEnvOpt <- tryGetNodePcEnv (Visit (Addr split) restrs2) Nothing
    whenJustThen prevPcEnvOpt $ \prevPcEnv -> do
        let (_, prevEnv) = prevPcEnv
        memCalls <- scanMemCalls prevEnv >>= addLoopMemCalls split
        let add name ty = do
                let name2 = printf "%s_loop_at_%s" name (prettyNodeId (Addr split))
                addVarRestrWithMemCalls name2 ty memCalls
        (env, consts) <- flip runStateT S.empty $ flip M.traverseWithKey prevEnv $ \(name, ty) _v -> do
            let checkConst = case ty of
                    ExprTypeHtd -> True
                    ExprTypeDom -> True
                    _ -> False
            isSyntConst <- if checkConst then isSyntacticConstant name ty split else return False
            if isSyntConst
                then do
                    modify $ S.insert (name, ty)
                    return $ prevEnv ! (name, ty)
                else do
                    NotSplit . nameS <$> lift (add (name.unwrap ++ "_after") ty)
        env' <- flip M.traverseWithKey env $ \(name, ty) v -> do
            if S.member (name, ty) consts
                then return v
                else do
                    v' <- varRepRequest name ty VarRepRequestKindLoop (Visit (Addr split) restrs) env
                    return $ maybe v Split v'
        pc' <- smtExprE boolT . NotSplit . nameS <$> add "pc_of" boolT
        return $ Just (pc', env')

getArcPcEnvs :: MonadRepGraph m => NodeAddr -> Visit -> m [Maybe (Expr, ExprEnv)]
getArcPcEnvs n visit2 = do
    r <- runExceptT $ do
        prevs <- askPrevs visit2 <&> filter (\visit -> visit.nodeId == Addr n)
        ensureM $ length prevs <= 1
        for prevs $ \visit -> getArcPcEnv visit visit2
    case r of
        Right x -> return x
        Left (TooGeneral { split }) -> do
            let specs =
                    [ Visit
                        { nodeId = visit2.nodeId
                        , restrs = spec
                        }
                    | spec <- specialize visit2 split
                    ]
            concat <$> traverse (getArcPcEnvs n) specs

getArcPcEnv :: MonadRepGraphE m => Visit -> Visit -> m (Maybe (Expr, ExprEnv))
getArcPcEnv visit' otherVisit = do
    (_tag, restrsOpt) <- getTagVCount visit' Nothing
    case restrsOpt of
        Nothing -> return Nothing
        Just restrs -> do
            let visit = Visit
                    { nodeId = visit'.nodeId
                    , restrs
                    }
            opt <- liftRepGraph $ use $ #arcPcEnvs % at visit
            case opt of
                Just r -> return $ r !? otherVisit.nodeId
                Nothing -> do
                    pcEnvOpt <- tryGetNodePcEnv visit Nothing
                    whenJustThen pcEnvOpt $ \_ -> do
                        runPreEmitNodeHook visit
                        arcs <- emitNode visit
                        let arcs' = M.fromList [ (cont, (pc, env)) | (cont, pc, env) <- arcs ]
                        liftRepGraph $ #arcPcEnvs %= M.insert visit arcs'
                        return $ arcs' !? otherVisit.nodeId

getTagVCount :: MonadRepGraphE m => Visit -> Maybe Tag -> m (Tag, Maybe [Restr])
getTagVCount visit tagOpt = do
    tag <- whenNothing tagOpt $ askNodeTag $ nodeAddrFromNodeId visit.nodeId
    vcountWithReachable <- fmap catMaybes $ for visit.restrs $ \restr -> runMaybeT $ do
        reachable <- askIsNonTriviallyReachableFrom restr.nodeAddr visit.nodeId
        tag' <- askNodeTag restr.nodeAddr
        guard $ tag' == tag
        return (restr, reachable)
    let done = flip any vcountWithReachable $ \(restr, reachable) ->
            not reachable && not (hasZeroVC restr.visitCount)
    if done
        then return (tag, Nothing)
        else do
            let vcount = sort [ restr | (restr, reachable) <- vcountWithReachable, reachable ]
            runMaybeT $ do
                nodeAddr <- hoistMaybe $ preview #_Addr visit.nodeId
                loopId <- MaybeT $ askLoopHead nodeAddr
                for_ vcount $ \restr -> do
                    loopIdOpt' <- askLoopHead restr.nodeAddr
                    when (loopIdOpt' == Just loopId && isOptionsVC restr.visitCount) $ do
                        throwError $ TooGeneral { split = restr.nodeAddr }
            return (tag, Just vcount)

warmPcEnvCache :: MonadRepGraph m => VisitWithTag -> m ()
warmPcEnvCache visitWithTag = do
    let go = do
            visit <- get
            prevs <- askPrevs visit
            let f prev = do
                    present <- liftRepGraph $ use $ #nodePcEnvs % to (M.member (VisitWithTag prev visitWithTag.tag))
                    if present
                        then return False
                        else do
                            tvc <- getTagVCount prev Nothing
                            return $ tvc == (visitWithTag.tag, Just visit.restrs)
            prevs' <- runExceptT $ filterM f prevs
            case prevs' of
                Right (visit':_) -> do
                    tell [visit']
                    put visit'
                _ -> do
                    hoistMaybe Nothing
    (_, prevChain) <- evalRWST (runMaybeT (replicateM iters go)) () visitWithTag.visit
    ensureM $ length prevChain < iters
    for_ (reverse prevChain) $ \visit -> do
        getNodePcEnv visit (Just visitWithTag.tag)
  where
    iters = 5000

emitNode :: MonadRepGraphE m => Visit -> m [(NodeId, Expr, ExprEnv)]
emitNode visit = do
    (pc, env) <- fromJust <$> tryGetNodePcEnv visit Nothing
    let nodeAddr = nodeAddrFromNodeId visit.nodeId
    tag <- askNodeTag nodeAddr
    node <- liftRepGraph $ gview $ #problem % #nodes % at nodeAddr % unwrapped
    if pc == falseE
        then return [ (cont, falseE, M.empty) | cont <- node ^.. nodeConts ]
        else case node of
            NodeCond condNode | condNode.left == condNode.right -> do
                return [(condNode.left, pc, env)]
            NodeCond condNode | condNode.expr == trueE -> do
                return [(condNode.left, pc, env), (condNode.right, falseE, env)]
            NodeBasic basicNode -> do
                updates <- for basicNode.varUpdates $ \update -> do
                    val <- case update.expr.value of
                        ExprValueVar name -> do
                            return $ env ! (name, update.expr.ty)
                        _ -> do
                            let name = localName update.varName visit
                            withEnv env $ addLocalDef () () name update.expr
                    return ((update.varName, update.ty), val)
                let env' = M.union (M.fromList updates) env
                return [(basicNode.next, pc, env')]
            NodeCond condNode -> do
                let name = condName visit
                freshName <- getFreshIdent name
                let cond = varE boolT freshName
                def <- withEnv env $ addLocalDef () () name condNode.expr
                let env' = M.insert (freshName, boolT) def env
                let lpc = andE cond pc
                let rpc = andE (notE cond) pc
                return [(condNode.left, lpc, env'), (condNode.right, rpc, env')]
            NodeCall callNode -> do
                let name = successName callNode.functionName visit
                success <- smtExprE boolT . NotSplit . nameS <$> addVar name boolT
                sigs <- liftRepGraph $ gview #functionSigs
                let sig = sigs $ WithTag tag callNode.functionName
                ins <- fmap M.fromList $ for (zip sig.input callNode.input) $ \(funArg, callArg) -> do
                    v <- withEnv env $ convertExpr callArg
                    return ((funArg.name, funArg.ty), v)
                memCalls <- addMemCall callNode.functionName <$> scanMemCalls ins
                let m = do
                        for_ (zip callNode.output sig.output) $ \(x, y) -> do
                            ensureM $ x.ty == y.ty
                            var <- addVarRestrWithMemCalls (localName x.name visit) x.ty memCalls
                            modify $ M.insert (x.name, x.ty) (NotSplit (nameS var))
                            tell [((y.name, y.ty), NotSplit (nameS var))]
                        for (zip callNode.output sig.output) $ \(x, y) -> do
                            opt <- get >>= varRepRequest x.name x.ty VarRepRequestKindCall visit
                            whenJust_ opt $ \v -> do
                                modify $ M.insert (x.name, x.ty) (Split v)
                                tell [((y.name, x.ty), Split v)]
                (_, env', outs') <- runRWST m () env
                let outs = M.fromList outs'
                addFunc callNode.functionName ins outs success visit
                return [(callNode.next, pc, env')]

isSyntacticConstant :: MonadRepGraph m => Ident -> ExprType -> NodeAddr -> m Bool
isSyntacticConstant origName ty split = do
    hasInnerLoop <- getHasInnerLoop split
    if hasInnerLoop
        then return False
        else do
            loopSet <- askLoopBody split
            let go visit safe (name, n) = do
                    node <- liftRepGraph $ gview $ #problem % #nodes % at n % unwrapped
                    let newName = name
                    newName' <- case node of
                        NodeCall callNode ->
                            if Argument name ty `elem` callNode.output
                            then throwError False
                            else return newName
                        NodeBasic basicNode -> do
                            let updateExprs =
                                    [ u.expr
                                    | u <- basicNode.varUpdates
                                    , Argument u.varName u.ty == Argument name ty
                                    ]
                            let updateExprsOpt = for updateExprs $ \v -> case v.value of
                                    ExprValueVar i -> Just i
                                    _ -> Nothing
                            case updateExprsOpt of
                                Nothing -> throwError False
                                Just updateExprs' -> case updateExprs' of
                                    name':_ -> return name'
                                    _ -> return newName
                        _ -> return newName
                    allPreds <- askPreds $ Addr n
                    let preds = [ (newName', n2) | n2 <- S.toAscList allPreds, n2 `S.member` loopSet ]
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
            runExceptT (go mempty (S.singleton (origName, split)) (origName, split)) >>= \case
                Left r -> return r

--

-- -- TODO
-- askCont :: MonadRepGraph m => Visit -> m Visit
-- askCont visit = do
--     let nodeAddr = nodeAddrFromNodeId visit.nodeId
--     conts <- liftRepGraph $ asks $ toListOf $ #problem % #nodes % at nodeAddr % unwrapped % nodeConts
--     let [cont] = conts
--     let p = any (\restr -> restr.nodeAddr == nodeAddr) visit.restrs
--     return $ Visit
--         { nodeId = cont
--         , restrs = if p then fromJust (incrVCs visit.restrs nodeAddr 1) else visit.restrs
--         }

-- -- TODO
-- convertInnerExprWithPcEnv :: MonadRepGraphE m => Expr -> Visit -> Maybe Tag -> m Expr
-- convertInnerExprWithPcEnv expr visit tag = do
--     pcEnv <- tryGetNodePcEnv visit tag
--     let Just (_pc, env) = pcEnv
--     withEnv env $ convertInnerExpr expr

-- -- TODO
-- postEmitNodeHook :: MonadRepGraphE m => Visit -> m ()
-- postEmitNodeHook visit = do
--     let n = nodeAddrFromNodeId visit.nodeId
--     tag <- askNodeTag n
--     when (tag == C) $ do
--         node <- liftRepGraph $ gview $ #problem % #nodes % at n % unwrapped
--         upds <- fmap catMaybes $ for (node ^.. foldExprs % getMemAccess) $ \acc -> case acc.kind of
--             MemOpKindUpdate -> Just <$> convertInnerExprWithPcEnv acc.addr visit Nothing
--             _ -> return Nothing
--         case upds of
--             [] -> return ()
--             _ -> error "unexpected"
