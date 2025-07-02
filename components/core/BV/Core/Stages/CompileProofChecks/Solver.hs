{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module BV.Core.Stages.CompileProofChecks.Solver
    ( MonadSolver (..)
    , Name (..)
    , NameHint
    , SMTEnv
    , SolverEnv
    , SolverOutput
    , SolverState
    , addDef
    , addDefNoSplit
    , addSplitMemVar
    , addVar
    , addVarRestr
    , assertFact
    , finalizeSolver
    , getDef
    , initSolver
    , initSolverEnv
    , initSolverState
    , mergeEnvsPcs
    , nameS
    , smtExprM
    , smtExprNoSplitM
    , toSmtExprM
    , tryGetDef
    , withEnv
    , withoutEnv
    ) where

import BV.Core.Logic
import BV.Core.Stages.CompileProofChecks.Structs
import BV.Core.Stages.Utils
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils
import BV.SMTLIB2.SExpr

import Control.DeepSeq (NFData)
import Control.Monad (unless, when, (>=>))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.RWS (MonadTrans (lift), RWS, modify)
import Control.Monad.State (execStateT, get)
import Control.Monad.Writer (tell)
import Data.Foldable (for_)
import Data.List (nub, sortOn)
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString (..))
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=), (<<%=))
import Text.Printf (printf)

{-# ANN module ("HLint: ignore" :: String) #-}

--

-- TODO
cheatMemDoms :: Bool
cheatMemDoms = True

--

class MonadStructs m => MonadSolver m where
    liftSolver :: RWS SolverEnv SolverOutput SolverState a -> m a

instance MonadSolver m => MonadSolver (ReaderT r m) where
    liftSolver = lift . liftSolver

instance MonadSolver m => MonadSolver (ExceptT e m) where
    liftSolver = lift . liftSolver

data SolverEnv
  = SolverEnv
      { rodata :: ROData
      }
  deriving (Eq, Generic, NFData, Ord, Show)

type SolverOutput = [SExprWithPlaceholders]

data SolverState
  = SolverState
      { namesUsed :: Set Name
      , externalNames :: Set Name
      , pvalids :: Map S (Map (PValidType, Name, PValidKind) S)
      , ptrs :: Map S Name
      , cachedExprs :: Map S Name
      , cachedExprNames :: Set Name
      , defs :: Map Name S
      , doms :: Set (S, S, S)
      , modelVars :: Set Name
      , modelExprs :: Set SExprWithPlaceholders
      , stackEqsStackEqImpliesCheck :: Map S (Maybe S)
      , stackEqsImpliesStackEq :: Map (Expr, Expr, Expr) Name
      , tokenTokens :: Map String Name
      , tokenVals :: Map S String
      , smtDerivedOps :: Map (Op, Integer) String
      }
  deriving (Eq, Generic, NFData, Ord, Show)

initSolverEnv :: ROData -> SolverEnv
initSolverEnv rodata = SolverEnv
    { rodata
    }

initSolverState :: SolverState
initSolverState = SolverState
    { namesUsed = S.empty
    , externalNames = S.empty
    , pvalids = M.empty
    , ptrs = M.empty
    , cachedExprs = M.empty
    , cachedExprNames = S.empty
    , defs = M.empty
    , doms = S.empty
    , modelVars = S.empty
    , modelExprs = S.empty
    , stackEqsStackEqImpliesCheck = M.empty
    , stackEqsImpliesStackEq = M.empty
    , tokenTokens = M.empty
    , tokenVals = M.empty
    , smtDerivedOps = M.empty
    }

--

send :: MonadSolver m => SExprWithPlaceholders -> m ()
send s = liftSolver $ tell [s]

--

initSolver :: MonadSolver m => m ()
initSolver = do
    addRODataDef

finalizeSolver :: MonadSolver m => m ()
finalizeSolver = do
    addPValidDomAssertions

--

type SMTEnv = Map (Ident, ExprType) SMT

withEnv :: SMTEnv -> ReaderT SMTEnv m a -> m a
withEnv = flip runReaderT

withoutEnv :: ReaderT SMTEnv m a -> m a
withoutEnv = flip runReaderT mempty

--

type NameHint = String

newtype Name
  = Name { unwrap :: String }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance IsString Name where
    fromString = Name

nameS :: Name -> S
nameS name = symbolS name.unwrap

takeFreshName :: MonadSolver m => NameHint -> m Name
takeFreshName =
    return . sanitize
        >=> takeFreshNameIn #externalNames
        >=> takeFreshNameIn #namesUsed
        >=> return . Name
  where
    sanitize = map $ \c -> if c `elem` ("'#\"" :: String) then '_' else c
    takeFreshNameIn l = liftSolver . zoom l . takeFreshNameHere
    takeFreshNameHere hint = do
        taken <- get
        let name = chooseFreshName (flip S.member taken . Name) hint
        modify $ S.insert (Name name)
        return name

--

askRODataPtrs :: MonadSolver m => m [(Expr, ExprType)]
askRODataPtrs = do
    rodata <- liftSolver $ gview #rodata
    return
        [ (machineWordE range.addr, globalWrapperT (structT structName))
        | (structName, range) <- rodataStructNamesOf rodata
        ]

--

opS :: Op -> S
opS x = case x of
    OpPlus -> "bvadd"
    OpMinus -> "bvsub"
    OpTimes -> "bvmul"
    OpModulus -> "bvurem"
    OpDividedBy -> "bvudiv"
    OpBWAnd -> "bvand"
    OpBWOr -> "bvor"
    OpBWXOR -> "bvxor"
    OpAnd -> "and"
    OpOr -> "or"
    OpImplies -> "=>"
    OpEquals -> "="
    OpLess -> "bvult"
    OpLessEquals -> "bvule"
    OpSignedLess -> "bvslt"
    OpSignedLessEquals -> "bvsle"
    OpShiftLeft -> "bvshl"
    OpShiftRight -> "bvlshr"
    OpSignedShiftRight -> "bvashr"
    OpNot -> "not"
    OpBWNot -> "bvnot"
    OpTrue -> "true"
    OpFalse -> "false"
    OpUnspecifiedPrecond -> "unspecified-precond"
    OpIfThenElse -> "ite"
    OpMemDom -> "mem-dom"
    OpROData -> "rodata"
    OpImpliesROData -> "implies-rodata"
    OpTokenWordsAccess -> "select"
    OpTokenWordsUpdate -> "store"
    OpWordArrayAccess -> "select"
    OpWordArrayUpdate -> "store"

--

getDef :: MonadSolver m => Name -> m S
getDef name = liftSolver $ use $ #defs % at name % unwrapped

tryGetDef :: MonadSolver m => Name -> m (Maybe S)
tryGetDef name = liftSolver $ use $ #defs % at name

addDefInner :: MonadSolver m => NameHint -> Expr -> ReaderT SMTEnv m (Either Name SplitMem)
addDefInner nameHint expr = smtExprM expr >>= \case
    SMT s -> Left <$> do
        name <- takeFreshName nameHint
        unless (isTypeOmitted ty) $ do
            -- TODO
            -- ensureM $ isn't #_ExprValueVar expr.value
            send $ defineFunS name.unwrap [] (typeToSMT ty) s
            liftSolver $ #defs %= M.insert name s
            when (isTypeRepresentable ty) $ do
                liftSolver $ #modelVars %= S.insert name
        return name
    SMTSplitMem splitMem -> Right <$> do
        let add suffix ty' s = nameS <$>
                addDefNoSplit (nameHint ++ "_" ++ suffix) (smtExprE ty' (SMT s))
        split <- add "split" machineWordT splitMem.split
        top <- add "top" ty splitMem.top
        bottom <- add "bot" ty splitMem.bottom
        return $ SplitMem
            { split
            , top
            , bottom
            }
  where
    ty = expr.ty

addDef :: MonadSolver m => NameHint -> Expr -> ReaderT SMTEnv m SMT
addDef nameHint val =
    either (SMT . nameS) SMTSplitMem <$> addDefInner nameHint val

addDefNoSplit :: MonadSolver m => NameHint -> Expr -> ReaderT SMTEnv m Name
addDefNoSplit nameHint val =
    view (expecting #_Left) <$> addDefInner nameHint val

isTypeRepresentable :: ExprType -> Bool
isTypeRepresentable = \case
    ExprTypeWord _ -> True
    ExprTypeBool -> True
    ExprTypeToken -> True
    _ -> False

isTypeOmitted :: ExprType -> Bool
isTypeOmitted = \case
    ExprTypeHtd -> True
    ExprTypePms -> True
    _ -> False

compiledTokenType :: ExprType
compiledTokenType = wordT 64

typeToSMT :: ExprType -> S
typeToSMT = \case
    ExprTypeWord bits -> bitVecS bits
    ExprTypeWordArray { len, bits } -> ["Array", bitVecS len, bitVecS bits]
    ExprTypeBool -> boolS
    ExprTypeMem -> memSortS
    ExprTypeDom -> memDomSortS
    ExprTypeToken -> typeToSMT compiledTokenType

addVar :: MonadSolver m => NameHint -> ExprType -> m Name
addVar nameHint ty = do
    name <- takeFreshName nameHint
    unless (isTypeOmitted ty) $ do
        send $ declareFunS name.unwrap [] (typeToSMT ty)
        when (isTypeRepresentable ty) $ do
            liftSolver $ #modelVars %= S.insert name
    return name

addVarRestr :: MonadSolver m => NameHint -> ExprType -> m Name
addVarRestr = addVar

assertSMTFact :: MonadSolver m => S -> m ()
assertSMTFact = send . assertS

assertFact :: MonadSolver m => Expr -> ReaderT SMTEnv m ()
assertFact = smtExprNoSplitM >=> assertSMTFact

withSetSlot :: (MonadSolver m, Ord k) => Lens' SolverState (S.Set k) -> k -> m () -> m ()
withSetSlot l k m = do
    seen <- liftSolver $ use $ l % to (S.member k)
    unless seen m
    liftSolver $ l %= S.insert k

withMapSlot :: (MonadSolver m, Ord k) => Lens' SolverState (M.Map k v) -> k -> m v -> m v
withMapSlot l k m = do
    liftSolver (use (l % at k)) >>= \case
        Just v -> do
            return v
        Nothing -> do
            v <- m
            liftSolver $ l %= M.insert k v
            return v

noteModelExpr :: MonadSolver m => S -> ExprType -> m ()
noteModelExpr s ty = withSetSlot #modelExprs s $ do
    let sanitized = take 20 $ filter (`notElem` (" ()" :: String)) (showSExprWithPlaceholders s)
    withoutEnv $ addDef ("query_" ++ sanitized) (smtExprE ty (SMT s))
    return ()

maybeNoteModelExpr :: MonadSolver m => S -> ExprType -> [Expr] -> m ()
maybeNoteModelExpr s ty subexprs =
    when (isTypeRepresentable ty && not (all isTypeRepresentable (map (.ty) subexprs))) $ do
        noteModelExpr s ty

notePtr :: MonadSolver m => S -> m Name
notePtr p_s = withMapSlot #ptrs p_s $
    withoutEnv $ addDefNoSplit "ptr" (smtExprE machineWordT (SMT p_s))

noteMemDom :: MonadSolver m => S -> S -> S -> m ()
noteMemDom p d md = liftSolver $ #doms %= S.insert (p, d, md)

cacheLargeExpr :: MonadSolver m => S -> NameHint -> ExprType -> m S
cacheLargeExpr s nameHint ty =
    liftSolver (use (#cachedExprs % at s)) >>= \case
        Just name -> return $ nameS name
        Nothing ->
            if length (showSExprWithPlaceholders s) < 80
            then do
                return s
            else do
                name <- withoutEnv $ addDefNoSplit nameHint (smtExprE ty (SMT s))
                liftSolver $ do
                    #cachedExprs %= M.insert s name
                    #cachedExprNames %= S.insert name
                return $ nameS name

getToken :: MonadSolver m => String -> m SMT
getToken string = fmap (SMT . nameS) $ withMapSlot #tokenTokens string $ do
    n <- liftSolver $ (+)
        <$> use (#tokenTokens % to M.size)
        <*> use (#tokenVals % to M.size)
    k <- withoutEnv $ addDefNoSplit ("token_" ++ string) (numE compiledTokenType (toInteger (n + 1)))
    v <- getDef k
    liftSolver $ #tokenVals %= M.insert v string
    return k

getDerivedOp :: MonadSolver m => Op -> Integer -> m S
getDerivedOp op n = do
    fname <- withMapSlot #smtDerivedOps (op, n) $ do
        let fname = case op of
                OpCountLeadingZeroes -> printf "bvclz_%d" n
                OpWordReverse -> printf "bvrev_%d" n
        body <- case n of
                1 -> return $ case op of
                        OpCountLeadingZeroes -> iteS ("x" `eqS` binS "0") (binS "1") (binS "0")
                        OpWordReverse -> "x"
                _ -> do
                    let m = n `div` 2
                    topOp <- getDerivedOp op (n - m)
                    botOp <- getDerivedOp op m
                    let top = [ixS "extract" [intS (n - 1), intS m], "x"]
                        bot = [ixS "extract" [intS (m - 1), intS 0], "x"]
                        topApp = [topOp, top]
                        botApp = [botOp, bot]
                        topAppX = [ixS "zero_extend" [intS m], topApp]
                        botAppX = [ixS "zero_extend" [intS (n - m)], botApp]
                    return $ case op of
                            OpCountLeadingZeroes ->
                                iteS
                                    (top `eqS` intWithWidthS (n - m) 0)
                                    (bvaddS botAppX (intWithWidthS n m))
                                    topAppX
                            OpWordReverse ->
                                concatS botApp topApp
        send $ defineFunS fname [("x", bitVecS n)] (bitVecS n) body
        return fname
    return $ symbolS fname

foldAssocBalanced :: (a -> a -> a) -> [a] -> a
foldAssocBalanced f = go
  where
    go xs =
        let n = length xs
          in if n >= 4
                then
                    let (lhs, rhs) = splitAt (n `div` 2) xs
                     in f (go lhs) (go rhs)
                else
                    foldr1 f xs

addRODataDef :: MonadSolver m => m ()
addRODataDef = do
    roName <- takeFreshName "rodata"
    impRoName <- takeFreshName "implies-rodata"
    ensureM $ roName.unwrap == "rodata"
    ensureM $ impRoName.unwrap == "implies-rodata"
    rodataPtrs <- askRODataPtrs
    (roDef, impRoDef) <- case rodataPtrs of
        [] -> do
            return (trueS, trueS)
        _ -> do
            roWitness <- addVar "rodata-witness" word32T
            roWitnessVal <- addVar "rodata-witness-val" word32T
            ensureM $ roWitness.unwrap == "rodata-witness"
            ensureM $ roWitnessVal.unwrap == "rodata-witness-val"
            rodata <- liftSolver $ gview #rodata
            let eqs =
                    [ eqS ["load-word32", "m", p] v
                    | (p, v) <-
                        [ (machineWordS p, machineWordS v)
                        | (p, v) <- M.toAscList rodata.rodata
                        ] ++
                        [ (symbolS roWitness.unwrap, symbolS roWitnessVal.unwrap)
                        ]
                    ]
            assertSMTFact $ orNS
                [ andS
                    (bvuleS (machineWordS range.addr) (symbolS roWitness.unwrap))
                    (bvuleS (symbolS roWitness.unwrap) (machineWordS (range.addr + range.size - 1)))
                | range <- rodata.ranges
                ]
            assertSMTFact $ eqS
                (symbolS roWitness.unwrap `bvandS` machineWordS 3)
                (machineWordS 0)
            return (andNS eqs, last eqs)
    send $ defineFunS
        roName.unwrap
        [("m", typeToSMT ExprTypeMem)]
        boolS
        roDef
    send $ defineFunS
        impRoName.unwrap
        [("m", typeToSMT ExprTypeMem)]
        boolS
        impRoDef

-- TODO
addPValidDomAssertions :: MonadSolver m => m ()
addPValidDomAssertions = do
    ensureM cheatMemDoms
    return ()

addSplitMemVar :: MonadSolver m => S -> NameHint -> ExprType -> m SplitMem
addSplitMemVar split nameHint ty@ExprTypeMem = do
    bottom <- nameS <$> addVar (nameHint ++ "_bot") ty
    top <- nameS <$> addVar (nameHint ++ "_top") ty
    liftSolver $ #stackEqsStackEqImpliesCheck %= M.insert top Nothing
    return $ SplitMem
        { split
        , top
        , bottom
        }

-- TODO move to RepGraph
mergeEnvsPcs :: MonadSolver m => [(Expr, SMTEnv)] -> m (Expr, SMTEnv, Bool)
mergeEnvsPcs unfilteredPcEnvs = do
    let pcEnvs = filter (\(pc, _) -> pc /= falseE) unfilteredPcEnvs
    let pc = case pcEnvs of
            [] -> falseE
            _ -> foldAssocBalanced orE (nub (map fst pcEnvs))
    env <- mergeEnvs pcEnvs
    return (pc, env, length pcEnvs > 1)

-- TODO move to RepGraph
data CompatSMTComparisonKey
  = SMTComparisonKeySMT String
  | SMTComparisonKeySplitMem String String String
  deriving (Eq, Generic, Ord, Show)

-- TODO move to RepGraph
compatSMTComparisonKey :: SMT -> CompatSMTComparisonKey
compatSMTComparisonKey = \case
    SMT s -> SMTComparisonKeySMT $ showSExprWithPlaceholders s
    SMTSplitMem s -> SMTComparisonKeySplitMem
        (showSExprWithPlaceholders s.split)
        (showSExprWithPlaceholders s.top)
        (showSExprWithPlaceholders s.bottom)

-- TODO move to RepGraph
mergeEnvs :: MonadSolver m => [(Expr, SMTEnv)] -> m SMTEnv
mergeEnvs envs = do
    varEnvs <-
        fmap (foldr (M.unionWith (M.unionWith (<>))) M.empty . concat)
            $ for envs $ \(pc, env) -> do
                pc' <- withEnv env $ smtExprNoSplitM pc
                return $
                    [ M.singleton var (M.singleton val ([pc'] :: [S]))
                    | (var, val) <- M.toAscList env
                    ]
    let flattenVal valsByPc =
            let Just (valsByPcInit, (lastVal, _)) = unsnoc valsByPc
                f accVal (val, pcs) = smtIfThenElse (orCompat pcs) val accVal
             in foldl f lastVal valsByPcInit
    return $ fmap (flattenVal . sortOn (compatSMTComparisonKey . fst) . M.toAscList) varEnvs
  where
    orCompat = \case
        [x] -> x
        xs -> orNS xs

--

toSmtExprM :: MonadSolver m => Expr -> ReaderT SMTEnv m Expr
toSmtExprM expr = case expr.ty of
    ExprTypeRelWrapper -> case expr.value of
        ExprValueOp op args -> do
            args' <- for args toSmtExprM
            return $ Expr expr.ty (ExprValueOp op args')
        _ -> error ""
    _ -> do
        s <- smtExprM expr
        return $ smtExprE expr.ty s

smtExprNoSplitM :: MonadSolver m => Expr -> ReaderT SMTEnv m S
smtExprNoSplitM expr = view (expecting #_SMT) <$> smtExprM expr

smtExprM :: MonadSolver m => Expr -> ReaderT SMTEnv m SMT
smtExprM expr = do
    case expr.value of
        ExprValueOp op args -> case op of
            _ | op == OpWordCast || op == OpWordCastSigned -> do
                    let [v] = args
                    let ExprTypeWord bitsExpr = expr.ty
                    let ExprTypeWord bitsV = v.ty
                    ex <- smtExprNoSplitM v
                    return . SMT $ if
                        | bitsExpr == bitsV -> ex
                        | bitsExpr < bitsV -> [["_", "extract", intS (bitsExpr - 1), intS 0], ex]
                        | otherwise ->
                            case op of
                                OpWordCast -> [["_", "zero_extend", intS (bitsExpr - bitsV)], ex]
                                OpWordCastSigned -> [["_", "sign_extend", intS (bitsExpr - bitsV)], ex]
            _ | op == OpToFloatingPoint || op == OpToFloatingPointSigned ||
                op == OpToFloatingPointUnsigned || op == OpFloatingPointCast -> do
                    error "unsupported"
            _ | op == OpCountLeadingZeroes || op == OpWordReverse -> do
                    let [v] = args
                    v' <- smtExprNoSplitM v
                    op' <- lift $ getDerivedOp op (expr.ty ^. expecting #_ExprTypeWord)
                    return . SMT $ [op', v']
            _ | op == OpCountTrailingZeroes -> do
                    let [v] = args
                    smtExprM $ clzE (wordReverseE v)
            _ | op `elem` ([OpPValid, OpPGlobalValid, OpPWeakValid, OpPArrayValid] :: [Op]) -> do
                (htd, typ_expr, p, f) <- case op of
                    OpPArrayValid -> do
                        let [htd, typ_expr, p, num] = args
                        len <- toSmtExprM num
                        let f ty = PValidTypeArray { ty, len }
                        return (htd, typ_expr, p, f)
                    _ -> do
                        let [htd, typ_expr, p] = args
                        let f = PValidTypeType
                        return (htd, typ_expr, p, f)
                let (ExprValueType typ) = typ_expr.value
                let typ' = case op of
                        OpPGlobalValid -> globalWrapperT typ
                        _ -> typ
                let pvTy = f typ'
                let (ExprValueVar htdName) = htd.value
                htd_s <- gview $ at (htdName, htd.ty) % unwrapped % expecting #_SMT
                p_s <- smtExprNoSplitM p
                var <- lift $ addPValidsM htd_s pvTy p_s (pvalidKindFromOp op)
                return $ SMT var
            OpMemDom -> do
                let [p, dom] = args
                p' <- smtExprNoSplitM p
                dom' <- smtExprNoSplitM dom
                let md = [opS op, p', dom']
                noteMemDom p' dom' md
                return $ SMT $ if cheatMemDoms
                    then trueS
                    else md
            OpMemUpdate -> do
                let [m, p, v] = args;
                ensureM $ isWordT v.ty
                m_s <- smtExprM m
                p_s <- smtExprNoSplitM p
                v_s <- smtExprNoSplitM v
                smtExprMemupdM m_s p_s v_s v.ty
            OpMemAcc -> do
                let [m, p] = args;
                ensureM $ isWordT expr.ty
                m_s <- smtExprM m
                p_s <- smtExprNoSplitM p
                SMT <$> smtExprMemAccM m_s p_s expr.ty
            OpStackEqualsImplies -> do
                args' <- mapM smtExprM args
                let [SMT sp1, st1, SMT sp2, st2] = args'
                if sp1 == sp2 && st1 == st2
                    then return $ SMT $ trueS
                    else do
                        let SMTSplitMem st2SplitMem = st2
                        eq <- getStackEqImplies st2SplitMem.split st2SplitMem.top st1
                        return $ SMT $ andS (eqS sp1 sp2) eq
            OpImpliesStackEquals -> do
                let [sp1, st1, sp2, st2] = args
                eq <- addImpliesStackEqM sp1 st1 st2
                sp1' <- smtExprNoSplitM sp1
                sp2' <- smtExprNoSplitM sp2
                return $ SMT $ andS (eqS sp1' sp2') eq
            _ | op == OpIfThenElse -> do
                    let [cond, x, y] = args
                    cond' <- smtExprNoSplitM cond
                    x' <- smtExprM x
                    y' <- smtExprM y
                    return $ smtIfThenElse cond' x' y'
            OpHTDUpdate -> do
                lift $ SMT . nameS <$> addVar "update_htd" expr.ty
            OpEquals | (head args).ty == ExprTypeMem -> do
                args' <- for args smtExprNoSplitM
                let [x, y] = args'
                let s = ["mem-eq", x, y]
                noteModelExpr s boolT
                return $ SMT s
            OpEquals | (head args).ty == word32T -> do
                args' <- for args smtExprNoSplitM
                let [x, y] = args'
                let s = ["word32-eq", x, y]
                return $ SMT s
            _ -> do
                args' <- for args smtExprNoSplitM
                let op' = opS op
                let s = case args' of
                        [] -> op'
                        _ -> List $ [op'] ++ args'
                lift $ maybeNoteModelExpr s expr.ty args
                return $ SMT s
        ExprValueNum n -> do
            return . SMT $ intWithWidthS (wordTBits expr.ty) n
        ExprValueVar var -> do
            let envKey = (var, expr.ty)
            env <- ask
            return $ case env !? envKey of
                Just sexpr -> sexpr
                Nothing -> error $ "env miss: " ++ show envKey
        ExprValueSMTExpr sexpr -> do
            return sexpr
        ExprValueToken tok -> do
            getToken tok.unwrap

smtExprMemupdM :: MonadSolver m => SMT -> S -> S -> ExprType -> m SMT
smtExprMemupdM mSplit p v (typ@(ExprTypeWord bits)) = do
    case mSplit of
        SMTSplitMem splitMem -> do
            p' <- cacheLargeExpr p "memupd_pointer" word32T
            v' <- cacheLargeExpr v "memupd_val" typ
            top <- cacheLargeExpr splitMem.top "split_mem_top" memT
            top_upd <- smtExprMemupdM (SMT top) p' v' typ
            bot <- cacheLargeExpr splitMem.bottom "split_mem_bot" memT
            bot_upd <- smtExprMemupdM (SMT bot) p' v' typ
            return $ SMTSplitMem $ SplitMem
                { split = splitMem.split
                , top = iteS (bvuleS splitMem.split p') (top_upd ^. expecting #_SMT) top
                , bottom = iteS (bvuleS splitMem.split p') bot (bot_upd ^. expecting #_SMT)
                }
        SMT m -> case bits of
            8 -> do
                p' <- cacheLargeExpr p "memupd_pointer" word32T
                let p_align = bvandS p' (hexS "fffffffd")
                noteModelExpr p_align word32T
                noteModelExpr (loadWord32S m p_align) word32T
                return $ SMT $ storeWord8S m p' v
            32 -> do
                noteModelExpr (loadWord32S m p) typ
                noteModelExpr p word32T
                return $ SMT $ storeWord32S m p v
            64 -> do
                noteModelExpr (loadWord64S m p) typ
                noteModelExpr p word32T
                return $ SMT $ storeWord64S m p v

smtExprMemAccM :: MonadSolver m => SMT -> S -> ExprType -> m S
smtExprMemAccM mSplit p (typ@(ExprTypeWord bits)) = do
    case mSplit of
        SMTSplitMem splitMem -> do
            p' <- cacheLargeExpr p "memacc_pointer" word32T
            top_acc <- smtExprMemAccM (SMT splitMem.top) p' typ
            bot_acc <- smtExprMemAccM (SMT splitMem.bottom) p' typ
            return $ iteS (bvuleS splitMem.split p') top_acc bot_acc
        SMT m -> do
            let s = case bits of
                    8 -> loadWord8S m p
                    32 -> loadWord32S m p
                    64 -> loadWord64S m p
            noteModelExpr p word32T
            noteModelExpr s typ
            return s


smtIfThenElse :: S -> SMT -> SMT -> SMT
smtIfThenElse cond x y =
    case (x, y) of
        (SMT x', SMT y') -> SMT $ iteS cond x' y'
        _ ->
            let splitMemX = toSplitMem x
                splitMemY = toSplitMem y
                split =
                    if splitMemX.split == splitMemY.split
                    then splitMemX.split
                    else iteS cond splitMemX.split splitMemY.split
             in SMTSplitMem $ SplitMem
                    { split
                    , top = iteS cond splitMemX.top splitMemY.top
                    , bottom = iteS cond splitMemX.bottom splitMemY.bottom
                    }
  where
    toSplitMem = \case
        SMTSplitMem splitMem -> splitMem
        SMT s -> SplitMem
            { split = machineWordS 0
            , top = s
            , bottom = s
            }

addPValidsM :: MonadSolver m => S -> PValidType -> S -> PValidKind -> m S
addPValidsM = go False
  where
    go :: MonadSolver m => Bool -> S -> PValidType -> S -> PValidKind -> m S
    go recursion htd_s typ p_s kind = do
        case htd_s of
            List [kw, cond, l, r] | kw == symbolS "ite" -> do
                l' <- addPValidsM l typ p_s kind
                r' <- addPValidsM r typ p_s kind
                return $ iteS cond l' r'
            _ -> do
                alreadyIn <- liftSolver $ use $ #pvalids % to (M.member htd_s)
                when (not alreadyIn && not recursion) $ do
                    rodataPtrs <- askRODataPtrs
                    for_ rodataPtrs $ \(r_addr, r_typ) -> do
                        r_addr_s <- withoutEnv $ smtExprNoSplitM r_addr
                        var <- go True htd_s (PValidTypeType r_typ) r_addr_s PValidKindPGlobalValid
                        assertSMTFact var
                p <- notePtr p_s
                present <- liftSolver $ preuse $ #pvalids % at htd_s % #_Just % at (typ, p, kind) % #_Just
                case present of
                    Just x -> do
                        return x
                    Nothing -> do
                        var <- addVar "pvalid" boolT
                        liftSolver $ #pvalids %= M.insertWith (<>) htd_s mempty
                        others <- liftSolver $ #pvalids % at htd_s % unwrapped <<%= M.insert (typ, p, kind) (nameS var)
                        let pdata = smtify (typ, p, kind) (nameS var)
                        let (_, pdataKind, p', pv') = pdata
                        impl_al <- impliesE pv' <$> alignValidIneq typ p'
                        withoutEnv $ assertFact impl_al
                        for (sortOn snd (M.toAscList others)) $ \val@((_valPvTy, _valName, valPvKind), _valS) -> do
                            let kinds :: [PValidKind] = [valPvKind, pdataKind]
                            unless (PValidKindPWeakValid `elem` kinds && not (PValidKindPGlobalValid `elem` kinds)) $ do
                                do
                                    ass <- pvalidAssertion1 pdata (uncurry smtify val)
                                    ass_s <- withoutEnv $ smtExprNoSplitM ass
                                    assertSMTFact ass_s
                                do
                                    ass <- pvalidAssertion2 pdata (uncurry smtify val)
                                    ass_s <- withoutEnv $ smtExprNoSplitM ass
                                    assertSMTFact ass_s
                        return $ nameS var
    smtify (typ, p, kind) var = (typ, kind, smtExprE machineWordT (SMT $ nameS p), smtExprE boolT (SMT var))

--

addImpliesStackEqM :: MonadSolver m => Expr -> Expr -> Expr -> ReaderT SMTEnv m S
addImpliesStackEqM sp s1 s2 = do
    let k = (sp, s1, s2)
    lift (liftSolver (use (#stackEqsImpliesStackEq % at k))) >>= \case
        Just v -> return $ nameS v
        Nothing -> do
            addr <- addVar "stack-eq-witness" word32T
            assertSMTFact (eqS (bvandS (nameS addr) (hexS "00000003")) (hexS "00000000"))
            sp_smt <- smtExprNoSplitM sp
            assertSMTFact (bvuleS sp_smt (nameS addr))
            let ptr = smtExprE word32T (SMT (nameS addr))
            let eq = eqE (memAccE word32T ptr s1) (memAccE word32T ptr s2)
            stack_eq <- addDefNoSplit "stack-eq" eq
            liftSolver $ #stackEqsImpliesStackEq %= M.insert k stack_eq
            return (nameS stack_eq)

getStackEqImplies :: MonadSolver m => S -> S -> SMT -> ReaderT SMTEnv m S
getStackEqImplies split st_top other = do
    let (rhs, cond) = case other of
            SMTSplitMem splitMem -> (splitMem.top, bvuleS splitMem.split split)
            SMT other' -> (other', trueS)
    noteModelExpr (eqS st_top rhs) boolT
    mems <- lift $ getImmBasisMems st_top
    let [st_top_base] = S.toList mems
    let k = st_top_base
    old <- liftSolver $ use $ #stackEqsStackEqImpliesCheck % at k % unwrapped
    ensureM $ case old of
        Nothing -> True
        Just old' -> old' == rhs
    liftSolver $ #stackEqsStackEqImpliesCheck %= M.insert k (Just rhs)
    return $ impliesS cond (eqS st_top rhs)

getImmBasisMems :: MonadSolver m => S -> m (Set S)
getImmBasisMems mTop = execStateT (go mTop) S.empty
  where
    go m = case m of
        List (op:args) -> case () of
            _ | op == symbolS "ite" -> do
                let [_c, l, r] = args
                go l
                go r
            _ | op == symbolS "store-word32" || op == symbolS "store-word8" -> do
                let [m', _p, _v] = args
                go m'
            _ -> error ""
        _ -> do
            let Just sym = parseSymbolS m
            isCached <- lift $ liftSolver $ use $ #cachedExprNames % to (S.member (Name sym))
            if isCached
                then do
                    def <- lift $ liftSolver $ use $ #defs % at (Name sym) % unwrapped
                    go def
                else do
                    modify $ S.insert m
