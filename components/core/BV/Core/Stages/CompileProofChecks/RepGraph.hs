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

import Control.Applicative (asum)
import Control.DeepSeq (NFData)
import Control.Monad (filterM, guard, replicateM, unless, when)
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
import Data.List (intercalate, isPrefixOf, sort, tails)
import Data.List.Split (splitOn)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
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

type FunctionSignatures = WithTag Ident -> FunctionSignature

type RepGraphContext m = (MonadReader RepGraphEnv m, MonadState RepGraphState m)

class MonadSolver m => MonadRepGraph m where
    liftRepGraph :: (forall n. RepGraphContext n => n a) -> m a

instance MonadRepGraph m => MonadRepGraph (ReaderT r m) where
    liftRepGraph f = lift $ liftRepGraph f

instance MonadRepGraph m => MonadRepGraph (StateT s m) where
    liftRepGraph f = lift $ liftRepGraph f

instance (Monoid w, MonadRepGraph m) => MonadRepGraph (RWST r w s m) where
    liftRepGraph f = lift $ liftRepGraph f

instance MonadRepGraph m => MonadRepGraph (MaybeT m) where
    liftRepGraph f = lift $ liftRepGraph f

instance MonadRepGraph m => MonadRepGraph (ExceptT e m) where
    liftRepGraph f = lift $ liftRepGraph f

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
    }

initRepGraph :: MonadRepGraph m => m ()
initRepGraph = do
    addInputEnvs

--

withSetSlot :: (MonadRepGraph m, Ord k) => Lens' RepGraphState (S.Set k) -> k -> m () -> m ()
withSetSlot l k m = do
    seen <- liftRepGraph $ use $ l % to (S.member k)
    unless seen m
    liftRepGraph $ l %= S.insert k

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

incrVCs :: [Restr] -> NodeAddr -> Integer -> Maybe [Restr]
incrVCs vcount n incr =
    if isEmptyVC vc
    then Nothing
    else Just (fromMapVC (M.insert n vc m))
  where
    m = toMapVC vcount
    vc = incrVC incr (m ! n)

getPc :: MonadRepGraph m => Visit -> Maybe Tag -> m Expr
getPc visit tag = view (expecting _Right) <$> runExceptT (tryGetPc visit tag)

tryGetPc :: MonadRepGraphE m => Visit -> Maybe Tag -> m Expr
tryGetPc visit tag = tryGetNodePcEnv visit tag >>= \case
    Nothing -> return falseE
    Just (pc, env) -> withEnv env $ convertInnerExpr pc

convertInnerExprWithPcEnv :: MonadRepGraphE m => Expr -> Visit -> Maybe Tag -> m Expr
convertInnerExprWithPcEnv expr visit tag = do
    pcEnv <- tryGetNodePcEnv visit tag
    let Just (_pc, env) = pcEnv
    withEnv env $ convertInnerExpr expr

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
            warmPcEnvCacheM vt
            getNodePcEnvRawM vt

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

getInductVar :: MonadRepGraph m => EqHypInduct -> m Expr
getInductVar induct =
    fmap (smtExprE word32T . NotSplit . nameS)
        $ withMapSlot #inductVarEnv induct $
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

askCont :: MonadRepGraph m => Visit -> m Visit
askCont visit = do
    let nodeAddr = visit.nodeId ^. expecting #_Addr
    conts <- liftRepGraph $ asks $ toListOf $ #problem % #nodes % at nodeAddr % unwrapped % nodeConts
    let [cont] = conts
    let p = any (\restr -> restr.nodeAddr == nodeAddr) visit.restrs
    return $ Visit
        { nodeId = cont
        , restrs = if p then fromJust (incrVCs visit.restrs nodeAddr 1) else visit.restrs
        }

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
            #problem % #nodes % at (v.nodeId ^. expecting #_Addr) % unwrapped % expecting #_NodeCall % #functionName
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

-- TODO
addLocalDef :: MonadRepGraphE m => () -> () -> NameHint -> Expr -> ReaderT ExprEnv m MaybeSplit
addLocalDef _ _ = addDef

addVarRestrWithMemCalls :: MonadRepGraph m => NameHint -> ExprType -> Maybe MemCalls -> m Name
addVarRestrWithMemCalls nameHint ty memCallsOpt = do
    r <- addVarRestr nameHint ty
    when (isMemT ty) $ do
        liftRepGraph $ #memCalls %= M.insert (nameS r) (fromJust memCallsOpt)
    return r

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

addLoopMemCallsM :: MonadRepGraphE m => NodeAddr -> Maybe MemCalls -> m (Maybe MemCalls)
addLoopMemCallsM split = traverse $ \memCalls -> do
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

warmPcEnvCacheM :: MonadRepGraph m => VisitWithTag -> m ()
warmPcEnvCacheM visitWithTag = do
    let go = do
            n_vc <- get
            prevs <- lift $ lift $ askPrevs n_vc
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
        getNodePcEnv n_vc (Just visitWithTag.tag)
    return ()

getTagVCount :: MonadRepGraphE m => Visit -> Maybe Tag -> m (Tag, Maybe [Restr])
getTagVCount visit mtag = do
    tag <- maybe (askNodeTag (visit.nodeId ^. expecting #_Addr)) return mtag
    vcount_r <- catMaybes <$> for visit.restrs (\restr -> runMaybeT $ do
        reachable <- lift $ askIsNonTriviallyReachableFrom restr.nodeAddr visit.nodeId
        tag' <- lift $ askNodeTag restr.nodeAddr
        guard $ tag' == tag
        return $ (restr.nodeAddr, restr.visitCount, reachable)
        )
    let done = flip any vcount_r $ \(_split, count, r) -> not r && not (hasZeroVC count)
    if done
        then return (tag, Nothing)
        else do
            let vcount = sort [ (split, count) | (split, count, r) <- vcount_r, r ]
            runMaybeT $ do
                nodeAddr <- hoistMaybe $ preview #_Addr visit.nodeId
                loopId <- MaybeT $ askLoopHead nodeAddr
                for_ vcount $ \(split, visits) -> do
                    maybeloopId' <- askLoopHead split
                    when (maybeloopId' == Just loopId && isOptionsVC visits) $ do
                        throwError $ TooGeneral { split }
            return (tag, Just [ Restr x y | (x, y) <- vcount ])

varRepRequest :: MonadRepGraph m => Ident -> ExprType -> VarRepRequestKind -> Visit -> ExprEnv -> m (Maybe SplitMem)
varRepRequest nm typ kind n_vc env = runMaybeT $ do
    let hook = asmStackRepHook
    let n = n_vc.nodeId ^. expecting #_Addr
    addr <- MaybeT $ hook nm typ kind n
    addr_s <- lift $ withEnv env $ convertExpr addr
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
    tag <- lift $ askNodeTag n
    guard $ tag == Asm
    guard $ "stack" `isPrefixOf` nm.unwrap
    guard $ typ == ExprTypeMem
    guard $ kind /= VarRepRequestKindInit
    argRenames <- lift $ liftRepGraph $ gview #argRenames
    return $ varE word32T (argRenames (PairingEqSideQuadrant
        { tag
        , direction = PairingEqDirectionIn
        }) (Ident "r13"))

getNodePcEnvRawM :: MonadRepGraphE m => VisitWithTag -> m (Maybe (Expr, ExprEnv))
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
                        preds <- askPreds visitWithTag.visit.nodeId
                        for (S.toList preds) $ \n_prev -> do
                            tag <- askNodeTag n_prev
                            if tag /= visitWithTag.tag
                                then return []
                                else getArcPcEnvsM n_prev visitWithTag.visit
                    case pc_envs of
                        [] -> return Nothing
                        _ -> do
                            pc_envs' <- case visitWithTag.visit.nodeId of
                                Err -> do
                                    for pc_envs $ \(pc, env) -> do
                                        pc' <- withEnv env $ convertInnerExpr pc
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
                                    NotSplit v' | length (showSExprWithPlaceholders v') > 80 -> contract nm visitWithTag.visit v' typ
                                    _ -> return v
                            return $ Just (pc', env')

getLoopPcEnvM :: MonadRepGraphE m => NodeAddr -> [Restr] -> m (Maybe (Expr, ExprEnv))
getLoopPcEnvM split vcount = do
    let vcount2 = flip withMapVC vcount $ M.insert split (numberVC 0)
    prev_pc_envOpt <- tryGetNodePcEnv (Visit (Addr split) vcount2) Nothing
    case prev_pc_envOpt of
        Nothing -> return Nothing
        Just prev_pc_env -> do
            let (_, prev_env) = prev_pc_env
            mem_calls <- scanMemCalls prev_env >>= addLoopMemCallsM split
            let av nm typ = do
                    let nm2 = printf "%s_loop_at_%s" (nm :: String) (prettyNodeId (Addr split))
                    addVarRestrWithMemCalls nm2 typ mem_calls
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

getArcPcEnvsM :: MonadRepGraph m => NodeAddr -> Visit -> m [Maybe (Expr, ExprEnv)]
getArcPcEnvsM n n_vc2 = do
    r <- runExceptT $ do
        prevs <- askPrevs n_vc2 <&> filter (\n_vc -> n_vc.nodeId == Addr n)
        ensureM $ length prevs <= 1
        for prevs $ \n_vc -> getArcPcEnvM n_vc n_vc2
    case r of
        Right x -> return x
        Left (TooGeneral { split }) -> do
            let specs =
                    [ Visit
                        { nodeId = n_vc2.nodeId
                        , restrs = spec
                        }
                    | spec <- specialize n_vc2 split
                    ]
            concat <$> (for specs $ \spec -> getArcPcEnvsM n spec)

getArcPcEnvM :: MonadRepGraphE m => Visit -> Visit -> m (Maybe (Expr, ExprEnv))
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
                    tryGetNodePcEnv visit Nothing >>= \case
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
    tag <- askNodeTag n
    when (tag == C) $ do
        node <- liftRepGraph $ gview $ #problem % #nodes % at n % unwrapped
        let accs = toListOf (foldExprs % getMemAccess) node
        upd_ps <- catMaybes <$> (for accs $ \acc -> case acc.kind of
            MemOpKindUpdate -> Just <$> convertInnerExprWithPcEnv acc.addr visit Nothing
            _ -> return Nothing)
        case upd_ps of
            [] -> return ()
            _ -> error "unexpected"

emitNodeM :: MonadRepGraphE m => Visit -> m [(NodeId, Expr, ExprEnv)]
emitNodeM n = do
    (pc, env) <- fromJust <$> tryGetNodePcEnv n Nothing
    let nodeAddr = n.nodeId ^. expecting #_Addr
    tag <- askNodeTag nodeAddr
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
                            vname <- withEnv env $ addLocalDef () () name v
                            return vname
                    return (k, val)
                let env' = M.union (M.fromList upds) env
                return [(basicNode.next, pc, env')]
            NodeCond condNode -> do
                let name = condName n
                freshName <- getFreshIdent name
                let cond = varE boolT freshName
                def <- withEnv env $ addLocalDef () () name (app_eqs condNode.expr)
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
                    x <- withEnv env $ convertExpr (app_eqs callArg)
                    return ((funArg.name, funArg.ty), x))
                mem_calls' <- scanMemCalls ins
                let mem_calls = addMemCall callNode.functionName $ mem_calls'
                let m = do
                        for (zip callNode.output sig.output) $ \(Argument x typ, Argument y typ2) -> do
                            ensureM $ typ == typ2
                            let name = localName x n
                            var <- lift $ addVarRestrWithMemCalls name typ mem_calls
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
                addFunc callNode.functionName ins outs success n
                return $ [(callNode.next, pc, env')]

isSyntConstM :: forall m. MonadRepGraph m => Ident -> ExprType -> NodeAddr -> m Bool
isSyntConstM orig_nm typ split = do
    hasInnerLoop <- getHasInnerLoop split
    if hasInnerLoop
        then return False
        else do
            loop_set <- askLoopBody split
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
                    all_preds <- askPreds $ Addr n
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
