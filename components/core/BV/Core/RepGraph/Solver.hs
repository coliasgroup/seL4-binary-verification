{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module BV.Core.RepGraph.Solver
    ( ExprEnv
    , MonadRepGraphSolver (..)
    , MonadRepGraphSolverSend (..)
    , Name (..)
    , NameHint
    , PcEnv (..)
    , SolverEnv
    , SolverOutput
    , SolverState
    , addDef
    , addPValidDomAssertions
    , addSplitMemVar
    , addVar
    , addVarRestr
    , askModelExprs
    , askModelVars
    , assertFact
    , convertExpr
    , convertExprNoSplit
    , convertInnerExpr
    , initSolver
    , initSolverEnv
    , initSolverState
    , mergeEnvsPcs
    , nameS
    , tryGetDef
    , withEnv
    , withoutEnv
    ) where

import BV.Core.GenerateFreshName
import BV.Core.Logic
import BV.Core.Structs
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils (whenNothing)
import BV.SMTLIB2.SExpr
import BV.Utils

import Control.DeepSeq (NFData)
import Control.Monad (unless, when, (>=>))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (Reader, ReaderT (runReaderT), asks)
import Control.Monad.RWS (RWST, lift, modify, tell)
import Control.Monad.State (StateT, get)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Writer (WriterT, execWriterT)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.List (nub, sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString (..))
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=), (<<%=))
import Text.Printf (printf)

--

-- TODO
cheatMemDoms :: Bool
cheatMemDoms = True

--

class Monad m => MonadRepGraphSolverSend m where
    sendSExprWithPlaceholders :: SExprWithPlaceholders -> m ()

instance Monad m => MonadRepGraphSolverSend (WriterT SolverOutput m) where
    sendSExprWithPlaceholders s = tell [s]

class (MonadStructs m, MonadRepGraphSolverSend m) => MonadRepGraphSolver m where
    liftSolver :: StateT SolverState (Reader SolverEnv) a -> m a

instance (Monoid w, MonadRepGraphSolver m) => MonadRepGraphSolver (RWST r w s m) where
    liftSolver = lift . liftSolver

instance (Monoid w, MonadRepGraphSolverSend m) => MonadRepGraphSolverSend (RWST r w s m) where
    sendSExprWithPlaceholders = lift . sendSExprWithPlaceholders

instance MonadRepGraphSolver m => MonadRepGraphSolver (ReaderT r m) where
    liftSolver = lift . liftSolver

instance MonadRepGraphSolverSend m => MonadRepGraphSolverSend (ReaderT r m) where
    sendSExprWithPlaceholders = lift . sendSExprWithPlaceholders

instance MonadRepGraphSolver m => MonadRepGraphSolver (StateT s m) where
    liftSolver = lift . liftSolver

instance MonadRepGraphSolverSend m => MonadRepGraphSolverSend (StateT s m) where
    sendSExprWithPlaceholders = lift . sendSExprWithPlaceholders

instance MonadRepGraphSolver m => MonadRepGraphSolver (MaybeT m) where
    liftSolver = lift . liftSolver

instance MonadRepGraphSolverSend m => MonadRepGraphSolverSend (MaybeT m) where
    sendSExprWithPlaceholders = lift . sendSExprWithPlaceholders

instance MonadRepGraphSolver m => MonadRepGraphSolver (ExceptT e m) where
    liftSolver = lift . liftSolver

instance MonadRepGraphSolverSend m => MonadRepGraphSolverSend (ExceptT e m) where
    sendSExprWithPlaceholders = lift . sendSExprWithPlaceholders

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
      , modelExprs :: Map SExprWithPlaceholders (Name, ExprType)
      , stackEqsStackEqImpliesCheck :: Map S (Maybe S)
      , stackEqsImpliesStackEq :: Map (Expr, Expr, Expr) Name
      , tokenTokens :: Map Ident Name
      , tokenVals :: Map S Ident
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
    , modelExprs = M.empty
    , stackEqsStackEqImpliesCheck = M.empty
    , stackEqsImpliesStackEq = M.empty
    , tokenTokens = M.empty
    , tokenVals = M.empty
    , smtDerivedOps = M.empty
    }

--

send :: MonadRepGraphSolver m => SExprWithPlaceholders -> m ()
send = sendSExprWithPlaceholders

--

initSolver :: MonadRepGraphSolver m => m ()
initSolver = do
    addRODataDef

--

type ExprEnv = Map NameTy MaybeSplit

withEnv :: ExprEnv -> ReaderT ExprEnv m a -> m a
withEnv = flip runReaderT

withoutEnv :: ReaderT ExprEnv m a -> m a
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

takeFreshName :: MonadRepGraphSolver m => NameHint -> m Name
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
        let name = generateFreshName (flip S.member taken . Name) hint
        modify $ S.insert (Name name)
        return name

--

withMapSlot :: (MonadRepGraphSolver m, Ord k) => Lens' SolverState (M.Map k v) -> k -> m v -> m v
withMapSlot l k m = do
    opt <- liftSolver (use (l % at k))
    whenNothing opt $ do
        v <- m
        liftSolver $ l %= M.insert k v
        return v

--

askModelVars :: MonadRepGraphSolver m => m (Set Name)
askModelVars = liftSolver $ use #modelVars

askModelExprs :: MonadRepGraphSolver m => m (Map SExprWithPlaceholders (Name, ExprType))
askModelExprs = liftSolver $ use #modelExprs

askRODataPtrs :: MonadRepGraphSolver m => m [(Expr, ExprType)]
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

getDef :: MonadRepGraphSolver m => Name -> m S
getDef name = fromJust <$> tryGetDef name

tryGetDef :: MonadRepGraphSolver m => Name -> m (Maybe S)
tryGetDef name = liftSolver $ use $ #defs % at name

addDef :: MonadRepGraphSolver m => NameHint -> Expr -> ReaderT ExprEnv m MaybeSplit
addDef nameHint val =
    either (NotSplit . nameS) Split <$> addDefInner nameHint val

addDefNotSplit :: MonadRepGraphSolver m => NameHint -> Expr -> ReaderT ExprEnv m Name
addDefNotSplit nameHint val =
    view (expecting #_Left) <$> addDefInner nameHint val

addDefInner :: MonadRepGraphSolver m => NameHint -> Expr -> ReaderT ExprEnv m (Either Name SplitMem)
addDefInner nameHint expr = convertExpr expr >>= \case
    NotSplit s -> Left <$> do
        name <- takeFreshName nameHint
        unless (isTypeOmitted ty) $ do
            -- TODO
            -- ensureM $ isn't #_ExprValueVar expr.value
            send $ defineFunS name.unwrap [] (typeToSMT ty) s
            liftSolver $ #defs %= M.insert name s
            when (isTypeRepresentable ty) $ do
                liftSolver $ #modelVars %= S.insert name
        return name
    Split splitMem -> Right <$> do
        let add suffix ty' s = nameS <$>
                addDefNotSplit (nameHint ++ "_" ++ suffix) (smtExprE ty' (NotSplit s))
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

typeToSMT :: ExprType -> S
typeToSMT = \case
    ExprTypeWord bits -> bitVecS bits
    ExprTypeWordArray { len, bits } -> ["Array", bitVecS len, bitVecS bits]
    ExprTypeBool -> boolS
    ExprTypeMem -> memSortS
    ExprTypeDom -> memDomSortS
    ExprTypeToken -> typeToSMT compiledTokenType

compiledTokenType :: ExprType
compiledTokenType = wordT 64

addVar :: MonadRepGraphSolver m => NameHint -> ExprType -> m Name
addVar nameHint ty = do
    name <- takeFreshName nameHint
    unless (isTypeOmitted ty) $ do
        send $ declareFunS name.unwrap [] (typeToSMT ty)
        when (isTypeRepresentable ty) $ do
            liftSolver $ #modelVars %= S.insert name
    return name

addVarRestr :: MonadRepGraphSolver m => NameHint -> ExprType -> m Name
addVarRestr = addVar

assertSMTFact :: MonadRepGraphSolver m => S -> m ()
assertSMTFact = send . assertS

assertFact :: MonadRepGraphSolver m => Expr -> ReaderT ExprEnv m ()
assertFact = convertExprNoSplit >=> assertSMTFact

noteModelExpr :: MonadRepGraphSolver m => S -> ExprType -> m ()
noteModelExpr s ty = void $ withMapSlot #modelExprs s $ do
    let sanitized = take 20 $ filter (`notElem` (" ()" :: String)) (showSExprWithPlaceholders s)
    v <- withoutEnv $ addDefNotSplit ("query_" ++ sanitized) (smtExprE ty (NotSplit s))
    return (v, ty)

maybeNoteModelExpr :: MonadRepGraphSolver m => S -> ExprType -> [Expr] -> m ()
maybeNoteModelExpr s ty subexprs =
    when (isTypeRepresentable ty && not (all (isTypeRepresentable . (.ty)) subexprs)) $ do
        noteModelExpr s ty

notePtr :: MonadRepGraphSolver m => S -> m Name
notePtr p_s = withMapSlot #ptrs p_s $
    withoutEnv $ addDefNotSplit "ptr" (smtExprE machineWordT (NotSplit p_s))

noteMemDom :: MonadRepGraphSolver m => S -> S -> S -> m ()
noteMemDom p d md = liftSolver $ #doms %= S.insert (p, d, md)

cacheLargeExpr :: MonadRepGraphSolver m => S -> NameHint -> ExprType -> m S
cacheLargeExpr s nameHint ty =
    liftSolver (use (#cachedExprs % at s)) >>= \case
        Just name -> return $ nameS name
        Nothing ->
            if length (showSExprWithPlaceholders s) < 80
            then do
                return s
            else do
                name <- withoutEnv $ addDefNotSplit nameHint (smtExprE ty (NotSplit s))
                liftSolver $ do
                    #cachedExprs %= M.insert s name
                    #cachedExprNames %= S.insert name
                return $ nameS name

getToken :: MonadRepGraphSolver m => Ident -> m MaybeSplit
getToken ident = fmap (NotSplit . nameS) $ withMapSlot #tokenTokens ident $ do
    n <- liftSolver $ (+)
        <$> use (#tokenTokens % to M.size)
        <*> use (#tokenVals % to M.size)
    k <- withoutEnv $ addDefNotSplit ("token_" ++ ident.unwrap) (numE compiledTokenType (toInteger (n + 1)))
    v <- getDef k
    liftSolver $ #tokenVals %= M.insert v ident
    return k

--

addRODataDef :: MonadRepGraphSolver m => m ()
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
                        | (p, v) <- M.toList rodata.rodata
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
addPValidDomAssertions :: MonadRepGraphSolver m => m ()
addPValidDomAssertions = do
    ensureM cheatMemDoms
    return ()

--

addSplitMemVar :: MonadRepGraphSolver m => S -> NameHint -> ExprType -> m SplitMem
addSplitMemVar split nameHint ty@ExprTypeMem = do
    bottom <- nameS <$> addVar (nameHint ++ "_bot") ty
    top <- nameS <$> addVar (nameHint ++ "_top") ty
    liftSolver $ #stackEqsStackEqImpliesCheck %= M.insert top Nothing
    return $ SplitMem
        { split
        , top
        , bottom
        }

--

data PcEnv
  = PcEnv
      { pc :: Expr
      , env :: ExprEnv
      }
  deriving (Eq, Generic, NFData, Ord, Show)

mergeEnvsPcs :: MonadRepGraphSolver m => [PcEnv] -> m (PcEnv, Bool)
mergeEnvsPcs unfilteredPcEnvs = do
    let pcEnvs = filter (\pcEnv -> pcEnv.pc /= falseE) unfilteredPcEnvs
    let pc = case pcEnvs of
            [] -> falseE
            _ -> foldAssocBalanced orE (nub (pcEnvs ^.. folded % #pc))
    env <- mergeEnvs pcEnvs
    return (PcEnv pc env, length pcEnvs > 1)

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

mergeEnvs :: MonadRepGraphSolver m => [PcEnv] -> m ExprEnv
mergeEnvs envs = do
    varEnvs <-
        fmap (foldr (M.unionWith (M.unionWith (<>))) M.empty) $
            for envs $ \(PcEnv pc env) -> do
                pc' <- withEnv env $ convertExprNoSplit pc
                return $ fmap (\val -> M.singleton val [pc']) env
    return $ flattenCompat . sortOn (compatSMTComparisonKey . fst) . M.toList <$> varEnvs
  where
    flattenCompat valsByPc =
        let Just (valsByPcInit, (lastVal, _)) = unsnoc valsByPc
            f accVal (val, pcs) = convertThenElse (orCompat pcs) val accVal
         in foldl f lastVal valsByPcInit
    orCompat = \case
        [x] -> x
        xs -> orNS xs

data CompatSMTComparisonKey
  = SMTComparisonKeySMT String
  | SMTComparisonKeySplitMem String String String
  deriving (Eq, Generic, Ord, Show)

compatSMTComparisonKey :: MaybeSplit -> CompatSMTComparisonKey
compatSMTComparisonKey = \case
    NotSplit s -> SMTComparisonKeySMT $ showSExprWithPlaceholders s
    Split s -> SMTComparisonKeySplitMem
        (showSExprWithPlaceholders s.split)
        (showSExprWithPlaceholders s.top)
        (showSExprWithPlaceholders s.bottom)

--

-- TODO rename
convertInnerExpr :: MonadRepGraphSolver m => Expr -> ReaderT ExprEnv m Expr
convertInnerExpr expr = case expr.ty of
    ExprTypeRelWrapper -> case expr.value of
        ExprValueOp op args -> do
            args' <- traverse convertInnerExpr args
            return $ Expr expr.ty (ExprValueOp op args')
    _ -> smtExprE expr.ty <$> convertExpr expr

convertExprNoSplit :: MonadRepGraphSolver m => Expr -> ReaderT ExprEnv m S
convertExprNoSplit expr = view (expecting #_NotSplit) <$> convertExpr expr

convertExpr :: MonadRepGraphSolver m => Expr -> ReaderT ExprEnv m MaybeSplit
convertExpr expr = do
    case expr.value of
        ExprValueOp op args -> case op of
            _ | op == OpWordCast || op == OpWordCastSigned -> do
                    let [v] = args
                    let ExprTypeWord bitsExpr = expr.ty
                    let ExprTypeWord bitsV = v.ty
                    ex <- convertExprNoSplit v
                    return $ NotSplit $ if
                        | bitsExpr == bitsV -> ex
                        | bitsExpr < bitsV -> [["_", "extract", intS (bitsExpr - 1), intS 0], ex]
                        | otherwise ->
                            case op of
                                OpWordCast -> [["_", "zero_extend", intS (bitsExpr - bitsV)], ex]
                                OpWordCastSigned -> [["_", "sign_extend", intS (bitsExpr - bitsV)], ex]
            _ | op == OpToFloatingPoint || op == OpToFloatingPointSigned
                || op == OpToFloatingPointUnsigned || op == OpFloatingPointCast -> do
                    error "unsupported"
            _ | op == OpCountLeadingZeroes || op == OpWordReverse -> do
                    let [v] = args
                    let ExprTypeWord bits = expr.ty
                    v' <- convertExprNoSplit v
                    op' <- getDerivedOp op bits
                    return $ NotSplit [op', v']
            OpCountTrailingZeroes -> do
                    let [v] = args
                    convertExpr $ clzE (wordReverseE v)
            _ | op `elem` ([OpPValid, OpPGlobalValid, OpPWeakValid, OpPArrayValid] :: [Op]) -> do
                (htd, tyExpr, p, f) <- case op of
                    OpPArrayValid -> do
                        let [htd, tyExpr, p, num] = args
                        len <- convertInnerExpr num
                        let f ty = PValidTypeArray { ty, len }
                        return (htd, tyExpr, p, f)
                    _ -> do
                        let [htd, tyExpr, p] = args
                        let f = PValidTypeType
                        return (htd, tyExpr, p, f)
                let ExprValueType ty = tyExpr.value
                let pvTy = f $ case op of
                        OpPGlobalValid -> globalWrapperT ty
                        _ -> ty
                let ExprValueVar htdName = htd.value
                htd' <- gview $ at (NameTy htdName htd.ty) % unwrapped % expecting #_NotSplit
                p' <- convertExprNoSplit p
                NotSplit <$> addPValids htd' pvTy p' (pvalidKindFromOp op)
            OpMemDom -> do
                let [p, dom] = args
                p' <- convertExprNoSplit p
                dom' <- convertExprNoSplit dom
                let md = [opS op, p', dom']
                noteMemDom p' dom' md
                return $ NotSplit $ if cheatMemDoms then trueS else md
            OpMemUpdate -> do
                let [m, p, v] = args;
                ensureM $ isWordT v.ty
                m' <- convertExpr m
                p' <- convertExprNoSplit p
                v' <- convertExprNoSplit v
                convertMemUpdate m' p' v' v.ty
            OpMemAcc -> do
                let [m, p] = args;
                ensureM $ isWordT expr.ty
                m' <- convertExpr m
                p' <- convertExprNoSplit p
                NotSplit <$> convertMemAccess m' p' expr.ty
            OpStackEqualsImplies -> do
                args' <- traverse convertExpr args
                let [NotSplit sp1, st1, NotSplit sp2, st2] = args'
                if sp1 == sp2 && st1 == st2
                    then return $ NotSplit trueS
                    else do
                        let Split st2SplitMem = st2
                        eq <- getStackEqImplies st2SplitMem.split st2SplitMem.top st1
                        return $ NotSplit $ andS (eqS sp1 sp2) eq
            OpImpliesStackEquals -> do
                let [sp1, st1, sp2, st2] = args
                eq <- addImpliesStackEq sp1 st1 st2
                sp1' <- convertExprNoSplit sp1
                sp2' <- convertExprNoSplit sp2
                return $ NotSplit $ andS (eqS sp1' sp2') eq
            OpIfThenElse -> do
                let [cond, x, y] = args
                convertThenElse
                    <$> convertExprNoSplit cond
                    <*> convertExpr x
                    <*> convertExpr y
            OpHTDUpdate -> do
                NotSplit . nameS <$> addVar "update_htd" expr.ty
            OpEquals | (head args).ty == ExprTypeMem -> do
                args' <- traverse convertExprNoSplit args
                let [x, y] = args'
                let s = ["mem-eq", x, y]
                noteModelExpr s boolT
                return $ NotSplit s
            OpEquals | (head args).ty == word32T -> do
                args' <- traverse convertExprNoSplit args
                let [x, y] = args'
                let s = ["word32-eq", x, y]
                return $ NotSplit s
            _ -> do
                args' <- traverse convertExprNoSplit args
                let op' = opS op
                let s = case args' of
                        [] -> op'
                        _ -> List $ [op'] ++ args'
                maybeNoteModelExpr s expr.ty args
                return $ NotSplit s
        ExprValueNum n -> do
            return $ NotSplit $ intWithWidthS (wordTBits expr.ty) n
        ExprValueVar var -> do
            let envKey = NameTy var expr.ty
            let err = error $ "env miss: " ++ show envKey
            asks $ fromMaybe err . M.lookup envKey
        ExprValueSMTExpr s -> do
            return s
        ExprValueToken tok -> do
            getToken tok

convertThenElse :: S -> MaybeSplit -> MaybeSplit -> MaybeSplit
convertThenElse cond x y =
    case (x, y) of
        (NotSplit x', NotSplit y') -> NotSplit $ iteS cond x' y'
        _ ->
            let xSplit = trivSplit x
                ySplit = trivSplit y
             in Split $ SplitMem
                    { split =
                        if xSplit.split == ySplit.split
                        then xSplit.split
                        else iteS cond xSplit.split ySplit.split
                    , top = iteS cond xSplit.top ySplit.top
                    , bottom = iteS cond xSplit.bottom ySplit.bottom
                    }
  where
    trivSplit = \case
        Split splitMem -> splitMem
        NotSplit s -> SplitMem
            { split = machineWordS 0
            , top = s
            , bottom = s
            }

getDerivedOp :: MonadRepGraphSolver m => Op -> Integer -> m S
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

convertMemUpdate :: MonadRepGraphSolver m => MaybeSplit -> S -> S -> ExprType -> m MaybeSplit
convertMemUpdate mMaybeSplit p v ty@(ExprTypeWord bits) = do
    case mMaybeSplit of
        Split splitMem -> do
            p' <- cacheLargeExpr p "memupd_pointer" word32T
            v' <- cacheLargeExpr v "memupd_val" ty
            top <- cacheLargeExpr splitMem.top "split_mem_top" memT
            topUpd <- fromNotSplit <$> convertMemUpdate (NotSplit top) p' v' ty
            bottom <- cacheLargeExpr splitMem.bottom "split_mem_bot" memT
            bottomUpd <- fromNotSplit <$> convertMemUpdate (NotSplit bottom) p' v' ty
            let f = iteS (bvuleS splitMem.split p')
            return $ Split $ SplitMem
                { split = splitMem.split
                , top = f topUpd top
                , bottom = f bottom bottomUpd
                }
        NotSplit m -> case bits of
            8 -> do
                p' <- cacheLargeExpr p "memupd_pointer" word32T
                let pAlign = bvandS p' (hexS "fffffffd")
                noteModelExpr pAlign word32T
                noteModelExpr (loadWord32S m pAlign) word32T
                return $ NotSplit $ storeWord8S m p' v
            32 -> do
                noteModelExpr (loadWord32S m p) ty
                noteModelExpr p word32T
                return $ NotSplit $ storeWord32S m p v
            64 -> do
                noteModelExpr (loadWord64S m p) ty
                noteModelExpr p word32T
                return $ NotSplit $ storeWord64S m p v

convertMemAccess :: MonadRepGraphSolver m => MaybeSplit -> S -> ExprType -> m S
convertMemAccess mMaybeSplit p ty@(ExprTypeWord bits) = do
    case mMaybeSplit of
        Split splitMem -> do
            p' <- cacheLargeExpr p "memacc_pointer" word32T
            topAcc <- convertMemAccess (NotSplit splitMem.top) p' ty
            botAcc <- convertMemAccess (NotSplit splitMem.bottom) p' ty
            return $ iteS (bvuleS splitMem.split p') topAcc botAcc
        NotSplit m -> do
            let f = case bits of
                    8 -> loadWord8S
                    32 -> loadWord32S
                    64 -> loadWord64S
            let s = f m p
            noteModelExpr p word32T
            noteModelExpr s ty
            return s

addImpliesStackEq :: MonadRepGraphSolver m => Expr -> Expr -> Expr -> ReaderT ExprEnv m S
addImpliesStackEq sp s1 s2 = fmap nameS $ withMapSlot #stackEqsImpliesStackEq (sp, s1, s2) $ do
    addr <- addVar "stack-eq-witness" word32T
    assertSMTFact $ eqS (bvandS (nameS addr) (hexS "00000003")) (hexS "00000000")
    sp' <- convertExprNoSplit sp
    assertSMTFact $ bvuleS sp' (nameS addr)
    let f = memAccE word32T (smtExprE word32T (NotSplit (nameS addr)))
    addDefNotSplit "stack-eq" $ eqE (f s1) (f s2)

getStackEqImplies :: MonadRepGraphSolver m => S -> S -> MaybeSplit -> ReaderT ExprEnv m S
getStackEqImplies split stTop other = do
    let (rhs, cond) = case other of
            Split splitMem -> (splitMem.top, bvuleS splitMem.split split)
            NotSplit otherNotSplit -> (otherNotSplit, trueS)
    noteModelExpr (eqS stTop rhs) boolT
    mems <- getImmBasisMems stTop
    let [k] = S.toList mems
    old <- liftSolver $ use $ #stackEqsStackEqImpliesCheck % at k % unwrapped
    case old of
        Nothing -> liftSolver $ #stackEqsStackEqImpliesCheck %= M.insert k (Just rhs)
        Just oldVal -> ensureM $ oldVal == rhs
    return $ impliesS cond (eqS stTop rhs)

getImmBasisMems :: MonadRepGraphSolver m => S -> m (Set S)
getImmBasisMems = execWriterT . go
  where
    go = \case
        List (op:args) -> if
            | op == symbolS "ite" -> do
                let [_c, l, r] = args
                go l
                go r
            | op == symbolS "store-word8" || op == symbolS "store-word32" -> do
                let [m, _p, _v] = args
                go m
        m -> do
            let Just sym = parseSymbolS m
            isCached <- lift $ liftSolver $ use $ #cachedExprNames % to (S.member (Name sym))
            if isCached
                then lift (getDef (Name sym)) >>= go
                else tell $ S.singleton m

addPValids :: MonadRepGraphSolver m => S -> PValidType -> S -> PValidKind -> m S
addPValids = go False
  where
    go recursion htd pvTy ptr pvKind = do
        case htd of
            List [op, cond, l, r] | op == symbolS "ite" -> do
                iteS cond
                    <$> addPValids l pvTy ptr pvKind
                    <*> addPValids r pvTy ptr pvKind
            _ -> do
                present <- liftSolver $ use $ #pvalids % to (M.member htd)
                when (not present && not recursion) $ do
                    rodataPtrs <- askRODataPtrs
                    for_ rodataPtrs $ \(rAddr, rTy) -> do
                        rAddr' <- withoutEnv $ convertExprNoSplit rAddr
                        var <- go True htd (PValidTypeType rTy) rAddr' PValidKindPGlobalValid
                        assertSMTFact var
                ptrName <- notePtr ptr
                opt <- liftSolver $ preuse $ #pvalids % at htd % #_Just % at (pvTy, ptrName, pvKind) % #_Just
                whenNothing opt $ do
                    var <- addVar "pvalid" boolT
                    liftSolver $ #pvalids %= M.insertWith (<>) htd mempty
                    others <- liftSolver $
                        #pvalids % at htd % unwrapped <<%= M.insert (pvTy, ptrName, pvKind) (nameS var)
                    let pdata = smtify (pvTy, ptrName, pvKind) (nameS var)
                    let (_, pdataPvKind, pdataPtr, pdataPv) = pdata
                    withoutEnv . assertFact . impliesE pdataPv =<< alignValidIneq pvTy pdataPtr
                    for_ (sortOn snd (M.toList others)) $ \val@((_valPvTy, _valName, valPvKind), _valS) -> do
                        let kinds :: [PValidKind] = [valPvKind, pdataPvKind]
                        unless (PValidKindPWeakValid `elem` kinds && PValidKindPGlobalValid `notElem` kinds) $ do
                            let applyAssertion f =
                                    f pdata (uncurry smtify val)
                                        >>= withoutEnv . convertExprNoSplit
                                        >>= assertSMTFact
                            applyAssertion pvalidAssertion1
                            applyAssertion pvalidAssertion2
                    return $ nameS var
    smtify (ty, p, kind) var = (ty, kind, smtExprE machineWordT (NotSplit (nameS p)), smtExprE boolT (NotSplit var))
