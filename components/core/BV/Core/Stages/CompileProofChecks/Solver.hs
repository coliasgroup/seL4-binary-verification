{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BV.Core.Stages.CompileProofChecks.Solver
    ( MonadSolver (..)
    , MonadStructs (..)
    , SolverEnv
    , SolverOutput
    , SolverState
    , initSolver
    , initSolverEnv
    , initSolverState
    , lookupStructForSolver
    , smtExprM
    , smtExprNoSplitM
    ) where

import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils
import BV.SMTLIB2.SExpr

import BV.Core.Arch
import BV.Core.Stages.Utils
import Control.DeepSeq (NFData)
import Control.Monad (unless, when)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.RWS (MonadRWS, MonadTrans (lift), RWS)
import Control.Monad.State (MonadState, State, get, modify)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing)
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString (..))
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=), (.=), (<<%=))
import Text.Printf (printf)

class MonadStructs m => MonadSolver m where
    liftSolver :: RWS SolverEnv SolverOutput SolverState a -> m a

lookupStructForSolver :: MonadSolver m => Ident -> m Struct
lookupStructForSolver ident = liftSolver $ gview $ #structs % at ident % unwrapped

instance MonadStructs m => MonadStructs (ReaderT r m) where
    lookupStruct = lift . lookupStruct

instance MonadSolver m => MonadSolver (ReaderT r m) where
    liftSolver = lift . liftSolver

data SolverEnv
  = SolverEnv
      { rodata :: ROData
      , structs :: Map Ident Struct
      }
  deriving (Eq, Generic, NFData, Ord, Show)

type SolverOutput = [SExprWithPlaceholders]

data SolverState
  = SolverState
      { namesUsed :: Set Name
      , namesUsedOrder :: Seq Name
      , externalNames :: Set Name
      , modelExprs :: Set SExprWithPlaceholders
      , smtDerivedOps :: Map (Op, Integer) String
      , defs :: Map Name S
      , modelVars :: Set Name
      , cachedExprs :: Map S Name
      , cachedExprNames :: Set Name
        --   , ptrs :: Map SMT SMT
      , ptrs :: Map S Name
      , pvalids :: Map S (Map (PValidType, Name, PValidKind) S)
      }
  deriving (Eq, Generic, NFData, Ord, Show)

type NameHint = String

newtype Name
  = Name { unwrap :: String }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance IsString Name where
    fromString = Name

nameS :: Name -> S
nameS name = symbolS name.unwrap

initSolverEnv :: ROData -> Map Ident Struct -> Problem -> SolverEnv
initSolverEnv rodata cStructs problem = SolverEnv
    { rodata
    , structs = augmentStructs rodata cStructs problem
    }

initSolverState :: SolverState
initSolverState = SolverState
    { namesUsed = mempty
    , namesUsedOrder = mempty
    , externalNames = mempty
    , modelExprs = mempty
    , smtDerivedOps = mempty
    , defs = mempty
    , modelVars = mempty
    , cachedExprs = mempty
    , cachedExprNames = mempty
    , ptrs = mempty
    , pvalids = mempty
    }

initSolver :: MonadSolver m => m ()
initSolver = do
    addRODataDefM

send :: MonadSolver m => SExprWithPlaceholders -> m ()
send sexpr = liftSolver $ tell [sexpr]

class Monad m => MonadStructs m where
    lookupStruct :: Ident -> m Struct

typeSizeWith :: Map Ident Struct -> ExprType -> Integer
typeSizeWith structs = go
  where
    go = \case
        ExprTypeWord { bits } ->
            let (bytes, 0) = bits `divMod` 8
            in bytes
        ExprTypeArray { ty, length } -> go ty * length
        ExprTypeStruct ident -> (structs ! ident).size
        ExprTypePtr _ -> archPtrSizeBytes

typeSize :: MonadReader SolverEnv m => ExprType -> m Integer
typeSize ty = do
    structs <- gview #structs
    return $ typeSizeWith structs ty

typeAlignWith :: Map Ident Struct -> ExprType -> Integer
typeAlignWith structs ty = case ty of
    ExprTypeWord { } -> typeSizeWith structs ty
    ExprTypeArray { ty } -> typeAlignWith structs ty
    ExprTypeStruct ident -> (structs ! ident).align
    ExprTypePtr _ -> archPtrSizeBytes

typeAlign :: MonadReader SolverEnv m => ExprType -> m Integer
typeAlign ty = do
    structs <- gview #structs
    return $ typeSizeWith structs ty

typeName :: ExprType -> String
typeName = go
  where
    join = intercalate " "
    go a = case a of
        ExprTypeBool -> "Bool"
        ExprTypeMem -> "Mem"
        ExprTypeDom -> "Dom"
        ExprTypeHtd -> "HTD"
        ExprTypePms -> "PMS"
        ExprTypeUnit -> "UNIT"
        ExprTypeType -> "Type"
        ExprTypeToken -> "Token"
        ExprTypeRelWrapper -> "RelWrapper"
        ExprTypeWord { bits } -> join ["Word", show bits]
        ExprTypeWordArray { length, bits } -> join ["WordArray", show length, show bits]
        ExprTypeArray { ty, length } -> join ["Array", go ty, show length]
        ExprTypeStruct ident -> join ["Struct", ident.unwrap]
        ExprTypePtr ty -> join ["Ptr", go ty]

globalWrapperStructName :: ExprType -> Ident
globalWrapperStructName ty = Ident $ printf "Global (%s)" (typeName ty)

globalWrapperStructWith :: Map Ident Struct -> ExprType -> Struct
globalWrapperStructWith structs ty = Struct
    { size = typeSizeWith structs ty
    , align = typeAlignWith structs ty
    , fields =
        [ ( "v"
          , StructField
                { ty
                , offset = 0
                }
          )
        ]
    }

globalWrapperT :: ExprType -> ExprType
globalWrapperT = structT . globalWrapperStructName

rodataStructNames :: ROData -> [Ident]
rodataStructNames rodata =
    [ Ident $ case rodata.ranges of
            [_] -> "rodata_struct"
            _ -> printf "rodata_struct_%d" i
    | (i :: Integer, _) <- zip [1..] rodata.ranges
    ]

rodataPtrsWith :: ROData -> [(Expr, ExprType)]
rodataPtrsWith rodata =
    [ (machineWordE range.addr, globalWrapperT (structT structName))
    | (structName, range) <- zip (rodataStructNames rodata) rodata.ranges
    ]

rodataPtrsM :: MonadReader SolverEnv m => m [(Expr, ExprType)]
rodataPtrsM = do
    rodata <- gview #rodata
    return $ rodataPtrsWith rodata

rodataStructsWith :: ROData -> Map Ident Struct
rodataStructsWith rodata =
    M.fromList
        [ let struct = Struct
                { size = range.size
                , align = 1
                , fields = M.empty
                }
           in (structName, struct)
        | (structName, range) <- zip (rodataStructNames rodata) rodata.ranges
        ]

-- rodataStructs :: MonadReader SolverEnv m => m (Map Ident Struct)
-- rodataStructs = do
--     rodata <- gview $ #rodata
--     return $ rodataStructsWith rodata

augmentStructs :: ROData -> Map Ident Struct -> Problem -> Map Ident Struct
augmentStructs rodata cStructs problem =
    nonGlobal <> global
  where
    rodataStructs = rodataStructsWith rodata
    nonGlobal = cStructs <> rodataStructs
    global = M.fromList
        [ (globalWrapperStructName ty, globalWrapperStructWith cStructs ty)
        | ty <- toWrap
        ]
    toWrap = problem.nodes ^.. folded % traverseTopLevelLevelExprs % foldExprs % afolding isPGlobalValid
    isPGlobalValid expr = case expr.value of
        ExprValueOp OpPGlobalValid args ->
            let [_, tyExpr, _] = args
                Expr { ty = ExprTypeType, value = ExprValueType ty } = tyExpr
             in Just ty
        _ -> Nothing

type SMTEnv = Map (Ident, ExprType) SMT

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
                                OpWordCast -> [["_", "zero_extend", intS (bitsExpr - bitsV), ex]]
                                OpWordCastSigned -> [["_", "sign_extend", intS (bitsExpr - bitsV), ex]]
            _ | op == OpToFloatingPoint || op == OpToFloatingPointSigned ||
                op == OpToFloatingPointUnsigned || op == OpFloatingPointCast -> do
                    error "unsupported"
            _ | op == OpCountLeadingZeroes || op == OpWordReverse -> do
                    let [v] = args
                    v' <- smtExprNoSplitM v
                    op' <- lift $ getSMTDerivedOpM op (expr.ty ^. expecting #_ExprTypeWord)
                    return . SMT $ [symbolS op', v']
            _ | op == OpCountTrailingZeroes -> do
                    let [v] = args
                    smtExprM $ clzE (wordReverseE v)
            _ | op == OpIfThenElse -> do
                    let [cond, x, y] = args
                    cond' <- smtExprNoSplitM cond
                    x' <- smtExprM x
                    y' <- smtExprM y
                    return $ smtIfThenElse cond' x' y'
            _ -> do
                -- undefined
                return $ SMT ["TODO"]
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
            undefined

getSMTDerivedOpM :: MonadSolver m => Op -> Integer -> m String
getSMTDerivedOpM op n = do
    opt <- liftSolver $ use $ #smtDerivedOps % at (op, n)
    case opt of
        Just fname -> return fname
        Nothing -> do
            let fname = case op of
                    OpCountLeadingZeroes -> printf "bvclz_%d" n
                    OpWordReverse -> printf "bvrev_%d" n
            body <- case n of
                    1 -> return $ case op of
                            OpCountLeadingZeroes -> iteS ("x" `eqS` hexS "0") (hexS "1") (hexS "0")
                            OpWordReverse -> "x"
                    _ -> do
                        let m = n `div` 2
                        topAppOp <- getSMTDerivedOpM op (n - m)
                        botAppOp <- getSMTDerivedOpM op m
                        let top = [ixS "extract" [intS (n - 1), intS m], "x"]
                            bot = [ixS "extract" [intS (m - 1), intS 0], "x"]
                            topApp = [symbolS topAppOp, top]
                            topAppX = [ixS "zero_extend" [intS m], topApp]
                            botApp = [symbolS botAppOp, bot]
                            botAppX = [ixS "zero_extend" [intS (n - m)], botApp]
                        return $ case op of
                                OpCountLeadingZeroes ->
                                    iteS
                                        (top `eqS` intWithWidthS (n - m) 0)
                                        (bvaddS botAppX (intWithWidthS n m))
                                        topAppX
                                OpWordReverse ->
                                    concatS botApp topApp
            send $ defineFunS fname [("x", bitVecS 1)] (bitVecS 1) body
            liftSolver $ modify $ #smtDerivedOps % at (op, n) ?~ fname
            return fname

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

smtNameM :: MonadSolver m => NameHint -> m Name
smtNameM baseName = do
    let sanitizedBaseName = baseName & map (\c -> if c `elem` ("'#\"" :: String) then '_' else c)
    postExt <- liftSolver $ zoom #externalNames $ getFreshName sanitizedBaseName
    postUsed <- liftSolver $ zoom #namesUsed $ getFreshName postExt.unwrap
    let name = postUsed
    return name
  where
    getFreshName :: MonadState (Set Name) n => NameHint -> n Name
    getFreshName hint = do
        taken <- get
        let name = Name $ chooseFreshName (flip S.member taken . Name) hint
        modify $ S.insert name
        return name

addDefEitherM :: MonadSolver m => NameHint -> Expr -> ReaderT SMTEnv m (Either Name SplitMem)
addDefEitherM nameHint val = do
    smt <- smtExprM val
    case smt of
        SMTSplitMem splitMem -> do
            let add nm typ smt' = do
                    let nameHint' = nameHint ++ "_" ++ nm
                    x <- addDefM nameHint' (smtExprE typ (SMT smt'))
                    return $ x ^. expecting #_SMT
            split' <- add "split" machineWordT splitMem.split
            top' <- add "top" val.ty splitMem.top
            bot' <- add "bot" val.ty splitMem.bottom
            return $ Right $ SplitMem
                { split = split'
                , top = top'
                , bottom = bot'
                }
        SMT smt' -> do
            name <- smtNameM nameHint
            if isSMTTypeOmitted val.ty
            then do
                return $ Left name
            else do
                case val.value of
                    ExprValueVar _ -> error ""
                    _ -> return ()
                let ty = smtType val.ty
                send $ defineFunS name.unwrap [] ty smt'
                liftSolver $ #defs %= M.insert name smt'
                when (typeRepresentable val.ty) $ do
                    liftSolver $ #modelVars %= S.insert name
                return $ Left $ name

addDefM :: MonadSolver m => NameHint -> Expr -> ReaderT SMTEnv m SMT
addDefM nameHint val = do
    addDefEitherM nameHint val <&> \case
        Left name -> SMT $ nameS name
        Right splitMem -> SMTSplitMem splitMem

addDefNoSplitM :: MonadSolver m => NameHint -> Expr -> ReaderT SMTEnv m Name
addDefNoSplitM nameHint val = do
    addDefEitherM nameHint val <&> view (expecting #_Left)

typeRepresentable :: ExprType -> Bool
typeRepresentable = \case
    ExprTypeWord _ -> True
    ExprTypeBool -> True
    ExprTypeToken -> True
    _ -> False

isSMTTypeOmitted :: ExprType -> Bool
isSMTTypeOmitted = \case
    ExprTypeHtd -> True
    ExprTypePms -> True
    _ -> False

smtType :: ExprType -> S
smtType = \case
    ExprTypeWord bits -> bitVecS bits
    ExprTypeWordArray { length, bits } -> ["Array", bitVecS length, bitVecS bits]
    ExprTypeBool -> boolS
    ExprTypeMem -> memSortS
    ExprTypeDom -> memDomSortS
    ExprTypeToken -> smtType tokenSmtType

tokenSmtType :: ExprType
tokenSmtType = wordT 64

addVarM :: MonadSolver m => NameHint -> ExprType -> m Name
addVarM nameHint ty = do
    name <- smtNameM nameHint
    unless (isSMTTypeOmitted ty) $ do
        let ty' = smtType ty
        send $ declareFunS name.unwrap [] ty'
        when (typeRepresentable ty) $ do
            liftSolver $ #modelVars %= S.insert name
    return $ name

addVarRestrM :: MonadSolver m => NameHint -> ExprType -> m Name
addVarRestrM = addVarM

addRODataDefM :: MonadSolver m => m ()
addRODataDefM = do
    roName <- smtNameM "rodata"
    impRoName <- smtNameM "implies-rodata"
    ensureM $ roName.unwrap == "rodata"
    ensureM $ impRoName.unwrap == "implies-rodata"
    rodataPtrs <- liftSolver rodataPtrsM
    (roDef, impRoDef) <- case rodataPtrs of
        [] -> do
            return $ (trueS, trueS)
        _ -> do
            roWitness <- smtNameM "rodata-witness"
            roWitnessVal <- smtNameM "rodata-witness-val"
            ensureM $ roWitness.unwrap == "rodata-witness"
            ensureM $ roWitnessVal.unwrap == "rodata-witness-val"
            rodata <- liftSolver $ gview #rodata
            let eq_vs =
                    [ (smtNum p 32, smtNum v 32)
                    | (p, v) <- M.toList rodata.rodata
                    ] ++
                    [ (symbolS roWitness.unwrap, symbolS roWitnessVal.unwrap)
                    ]
            let eqs =
                    [ eqS ["load-word32", "m", p] v
                    | (p, v) <- eq_vs
                    ]
            let ro_def = andNS eqs
            let ro_ineqs =
                    [ bvuleS (smtNum range.addr 32) (symbolS roWitness.unwrap)
                        `andS` bvuleS (smtNum (range.addr + range.size) 32) (symbolS roWitnessVal.unwrap)
                    | range <- rodata.ranges
                    ]
            let assns :: [S] =
                    [ orNS ro_ineqs
                    , bvandS (symbolS roWitness.unwrap) (machineWordS 3) `eqS` machineWordS 0
                    ]
            for assns $ \assn -> do
                assertFactSmtM assn
            let imp_ro_def = last eqs
            return $ (ro_def, imp_ro_def)
    send $ defineFunS
        roName.unwrap
        [("m", smtType ExprTypeMem)]
        boolS
        roDef
    send $ defineFunS
        impRoName.unwrap
        [("m", smtType ExprTypeMem)]
        boolS
        impRoDef

smtNum :: Integer -> Integer -> S
smtNum num bits = intWithWidthS bits num

-- smtNumTy :: ExprType -> S
-- smtNumTy = \case
--     ExprTypeWord bits -> intWithWidth bits

assertFactSmtM :: MonadSolver m => S -> m ()
assertFactSmtM fact = do
    send $ assertS fact

assertFactM :: MonadSolver m => Expr -> ReaderT SMTEnv m ()
assertFactM fact = do
    fact' <- smtExprNoSplitM fact
    send $ assertS fact'

noteModelExprM :: MonadSolver m => S -> ExprType -> m ()
noteModelExprM sexpr ty = do
    seen <- liftSolver $ use $ #modelExprs % to (S.member sexpr)
    unless seen $ do
        let s = take 20 $ filter (`notElem` (" ()" :: String)) (showSExprWithPlaceholders sexpr)
        let smtExpr = smtExprE ty (SMT sexpr)
        flip runReaderT M.empty $ addDefM ("query_" ++ s) smtExpr
        liftSolver $ #modelExprs %= S.insert sexpr

cacheLargeExprM :: MonadSolver m => S -> NameHint -> ExprType -> m S
cacheLargeExprM s nameHint typ = do
    liftSolver (use (#cachedExprs % at s)) >>= \case
        Just name -> do
            return $ nameS name
        Nothing -> do
            if length (showSExprWithPlaceholders s) < 80
            then do
                return s
            else do
                name <- withoutEnv $ addDefNoSplitM nameHint (smtExprE typ (SMT s))
                liftSolver $ do
                    #cachedExprs %= M.insert s name
                    #cachedExprNames %= S.insert name
                return $ nameS name

withoutEnv :: ReaderT SMTEnv m a -> m a
withoutEnv = flip runReaderT mempty

notePtrM :: MonadSolver m => S -> m Name
notePtrM p_s = do
    liftSolver (use (#ptrs % at p_s)) >>= \case
        Just p -> do
            return p
        Nothing -> do
            p <- withoutEnv $ addDefNoSplitM "ptr" (smtExprE machineWordT (SMT p_s))
            liftSolver $ #ptrs %= M.insert p_s p
            return p

data PValidType
  = PValidTypeType ExprType
  | PValidTypeArray
      { ty :: ExprType
      , length :: Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PValidKind
  = PValidKindGlobal
  deriving (Eq, Generic, NFData, Ord, Show)

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
                alreadyIn <- liftSolver $ use $ #pvalids % at htd_s % to isNothing
                when (not alreadyIn && not recursion) $ do
                    rodataPtrs <- liftSolver rodataPtrsM
                    for_ rodataPtrs $ \(r_addr, r_typ) -> do
                        r_addr_s <- withoutEnv $ smtExprNoSplitM r_addr
                        var <- go True htd_s (PValidTypeType r_typ) r_addr_s PValidKindGlobal
                        assertFactSmtM var
                p <- notePtrM p_s
                present <- liftSolver $ preuse $ #pvalids % at htd_s % #_Just % at (typ, p, kind) % #_Just
                case present of
                    Just x -> do
                        return x
                    Nothing -> do
                        var <- addVarM "pvalid" boolT
                        liftSolver $ #pvalids %= M.insertWith (<>) htd_s mempty
                        others <- liftSolver $ #pvalids % at htd_s % unwrapped <<%= M.insert (typ, p, kind) (nameS var)
                        undefined

pvalidAssertion1E :: (PValidType, PValidKind, Expr, Expr) -> (PValidType, PValidKind, Expr, Expr) -> Expr
pvalidAssertion1E (typ, k, p, pv) (typ2, k2, p2, pv2) =
    (pv `andE` pv2) `impliesE` (foldr1 orE ([cond1, cond2, out1, out2] :: [Expr]))
  where
    offs1 = p `minusE` p2
    cond1 = getSTypCondition offs1 typ typ2
    offs2 = p2 `minusE` p
    cond2 = getSTypCondition offs2 typ2 typ
    out1 = undefined
    out2 = undefined

getSTypCondition :: Expr -> PValidType -> PValidType -> Expr
getSTypCondition =
    undefined

-- end_addr :: Expr -> PValidType -> Expr
-- end_addr p typ = p `plusE` (sz `minusE` machineWordE 1)
--   where
--     sz = case typ of
--         PValidTypeArray { ty, length } -> machineWordE
--         timesE
--     if typ[0] == 'Array':
--         (_, typ, n) = typ
--         sz = mk_times (mk_word32 (typ.size ()), n)
--     else:
--         assert typ[0] == 'Type', typ
--         (_, typ) = typ
--         sz = mk_word32 (typ.size ())
--     return mk_plus (p, mk_minus (sz, mk_word32 (1)))
