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
    , addDefM
    , addDefNoSplitM
    , addSplitMemVarM
    , addVarM
    , addVarRestrM
    , assertFactM
    , finalizeSolver
    , getDefM
    , getDefOptM
    , initSolver
    , initSolverEnv
    , initSolverState
    , mergeEnvsPcs
    , nameS
    , smtExprM
    , smtExprNoSplitM
    , toSmtExprM
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
import Control.Monad (join, unless, when, (>=>))
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
      , tokenTokens :: Map String S
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
send sexpr = liftSolver $ tell [sexpr]

--

initSolver :: MonadSolver m => m ()
initSolver = do
    addRODataDefM

finalizeSolver :: MonadSolver m => m ()
finalizeSolver = do
    addPValidDomAssertionsM

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

tokenSmtType :: ExprType
tokenSmtType = wordT 64

getDefM :: MonadSolver m => Name -> m S
getDefM name = liftSolver $ use $ #defs % at name % unwrapped

getDefOptM :: MonadSolver m => Name -> m (Maybe S)
getDefOptM name = liftSolver $ use $ #defs % at name

addDefEitherM :: MonadSolver m => NameHint -> Expr -> ReaderT SMTEnv m (Either Name SplitMem)
addDefEitherM nameHint val = smtExprM val >>= \case
    SMT smt -> Left <$> do
        name <- takeFreshName nameHint
        unless (isSMTTypeOmitted val.ty) $ do
            -- TODO
            -- case val.value of
            --     ExprValueVar _ -> error ""
            --     _ -> return ()
            send $ defineFunS name.unwrap [] (smtType val.ty) smt
            liftSolver $ #defs %= M.insert name smt
            when (typeRepresentable val.ty) $ do
                liftSolver $ #modelVars %= S.insert name
        return name
    SMTSplitMem splitMem -> Right <$> do
        let add nm typ smt = nameS <$> addDefNoSplitM (nameHint ++ "_" ++ nm) (smtExprE typ (SMT smt))
        split <- add "split" machineWordT splitMem.split
        top <- add "top" val.ty splitMem.top
        bottom <- add "bot" val.ty splitMem.bottom
        return $ SplitMem
            { split
            , top
            , bottom
            }

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
    ExprTypeWordArray { len, bits } -> ["Array", bitVecS len, bitVecS bits]
    ExprTypeBool -> boolS
    ExprTypeMem -> memSortS
    ExprTypeDom -> memDomSortS
    ExprTypeToken -> smtType tokenSmtType

addVarM :: MonadSolver m => NameHint -> ExprType -> m Name
addVarM nameHint ty = do
    name <- takeFreshName nameHint
    unless (isSMTTypeOmitted ty) $ do
        let ty' = smtType ty
        send $ declareFunS name.unwrap [] ty'
        when (typeRepresentable ty) $ do
            liftSolver $ #modelVars %= S.insert name
    return $ name

addVarRestrM :: MonadSolver m => NameHint -> ExprType -> m Name
addVarRestrM = addVarM

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

maybeNoteModelExprM :: MonadSolver m => S -> ExprType -> [Expr] -> m ()
maybeNoteModelExprM sexpr typ subexprs = do
    when (typeRepresentable typ && not (all typeRepresentable (map (.ty) subexprs))) $ do
        noteModelExprM sexpr typ

notePtrM :: MonadSolver m => S -> m Name
notePtrM p_s = do
    liftSolver (use (#ptrs % at p_s)) >>= \case
        Just p -> do
            return p
        Nothing -> do
            p <- withoutEnv $ addDefNoSplitM "ptr" (smtExprE machineWordT (SMT p_s))
            liftSolver $ #ptrs %= M.insert p_s p
            return p

noteMemDomM :: MonadSolver m => S -> S -> S -> m ()
noteMemDomM p d md = liftSolver $ do
    #doms %= S.insert (p, d, md)

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

getTokenM :: MonadSolver m => String -> m SMT
getTokenM string = do
    present <- liftSolver $ use $ #tokenTokens % to (M.member string)
    unless present $ do
        n_x <- liftSolver $ use $ #tokenTokens % to M.size
        n_y <- liftSolver $ use $ #tokenVals % to M.size
        let n = n_x + n_y + 1
        v <- withoutEnv $ addDefNoSplitM ("token_" ++ string) (numE tokenSmtType (toInteger n))
        liftSolver $ do
            #tokenTokens %= M.insert string (nameS v)
            k <- use $ #defs % at v % unwrapped
            #tokenVals %= M.insert k string
    liftSolver $ use $ #tokenTokens % at string % unwrapped % to SMT

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
                    op' <- lift $ getSMTDerivedOpM op (expr.ty ^. expecting #_ExprTypeWord)
                    return . SMT $ [symbolS op', v']
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
                noteMemDomM p' dom' md
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
                lift $ SMT . nameS <$> addVarM "update_htd" expr.ty
            OpEquals | (head args).ty == ExprTypeMem -> do
                args' <- for args smtExprNoSplitM
                let [x, y] = args'
                let s = ["mem-eq", x, y]
                noteModelExprM s boolT
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
                lift $ maybeNoteModelExprM s expr.ty args
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
            getTokenM tok.unwrap

smtExprMemupdM :: MonadSolver m => SMT -> S -> S -> ExprType -> m SMT
smtExprMemupdM mSplit p v (typ@(ExprTypeWord bits)) = do
    case mSplit of
        SMTSplitMem splitMem -> do
            p' <- cacheLargeExprM p "memupd_pointer" word32T
            v' <- cacheLargeExprM v "memupd_val" typ
            top <- cacheLargeExprM splitMem.top "split_mem_top" memT
            top_upd <- smtExprMemupdM (SMT top) p' v' typ
            bot <- cacheLargeExprM splitMem.bottom "split_mem_bot" memT
            bot_upd <- smtExprMemupdM (SMT bot) p' v' typ
            return $ SMTSplitMem $ SplitMem
                { split = splitMem.split
                , top = iteS (bvuleS splitMem.split p') (top_upd ^. expecting #_SMT) top
                , bottom = iteS (bvuleS splitMem.split p') bot (bot_upd ^. expecting #_SMT)
                }
        SMT m -> case bits of
            8 -> do
                p' <- cacheLargeExprM p "memupd_pointer" word32T
                let p_align = bvandS p' (hexS "fffffffd")
                noteModelExprM p_align word32T
                noteModelExprM (loadWord32S m p_align) word32T
                return $ SMT $ storeWord8S m p' v
            32 -> do
                noteModelExprM (loadWord32S m p) typ
                noteModelExprM p word32T
                return $ SMT $ storeWord32S m p v
            64 -> do
                noteModelExprM (loadWord64S m p) typ
                noteModelExprM p word32T
                return $ SMT $ storeWord64S m p v

smtExprMemAccM :: MonadSolver m => SMT -> S -> ExprType -> m S
smtExprMemAccM mSplit p (typ@(ExprTypeWord bits)) = do
    case mSplit of
        SMTSplitMem splitMem -> do
            p' <- cacheLargeExprM p "memacc_pointer" word32T
            top_acc <- smtExprMemAccM (SMT splitMem.top) p' typ
            bot_acc <- smtExprMemAccM (SMT splitMem.bottom) p' typ
            return $ iteS (bvuleS splitMem.split p') top_acc bot_acc
        SMT m -> do
            let s = case bits of
                    8 -> loadWord8S m p
                    32 -> loadWord32S m p
                    64 -> loadWord64S m p
            noteModelExprM p word32T
            noteModelExprM s typ
            return s

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
                            OpCountLeadingZeroes -> iteS ("x" `eqS` binS "0") (binS "1") (binS "0")
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
            send $ defineFunS fname [("x", bitVecS n)] (bitVecS n) body
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

addRODataDefM :: MonadSolver m => m ()
addRODataDefM = do
    roName <- takeFreshName "rodata"
    impRoName <- takeFreshName "implies-rodata"
    ensureM $ roName.unwrap == "rodata"
    ensureM $ impRoName.unwrap == "implies-rodata"
    rodataPtrs <- askRODataPtrs
    (roDef, impRoDef) <- case rodataPtrs of
        [] -> do
            return $ (trueS, trueS)
        _ -> do
            roWitness <- addVarM "rodata-witness" word32T
            roWitnessVal <- addVarM "rodata-witness-val" word32T
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
                        `andS` bvuleS (symbolS roWitness.unwrap) (smtNum (range.addr + range.size - 1) 32)
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
                        let pdata = smtify (typ, p, kind) (nameS var)
                        let (_, pdataKind, p', pv') = pdata
                        impl_al <- impliesE pv' <$> alignValidIneq typ p'
                        withoutEnv $ assertFactM impl_al
                        for (sortOn snd (M.toAscList others)) $ \val@((_valPvTy, _valName, valPvKind), _valS) -> do
                            let kinds :: [PValidKind] = [valPvKind, pdataKind]
                            unless (PValidKindPWeakValid `elem` kinds && not (PValidKindPGlobalValid `elem` kinds)) $ do
                                do
                                    ass <- pvalidAssertion1 pdata (uncurry smtify val)
                                    ass_s <- withoutEnv $ smtExprNoSplitM ass
                                    assertFactSmtM ass_s
                                do
                                    ass <- pvalidAssertion2 pdata (uncurry smtify val)
                                    ass_s <- withoutEnv $ smtExprNoSplitM ass
                                    assertFactSmtM ass_s
                        return $ nameS var
    smtify (typ, p, kind) var = (typ, kind, smtExprE machineWordT (SMT $ nameS p), smtExprE boolT (SMT var))

--

addImpliesStackEqM :: MonadSolver m => Expr -> Expr -> Expr -> ReaderT SMTEnv m S
addImpliesStackEqM sp s1 s2 = do
    let k = (sp, s1, s2)
    lift (liftSolver (use (#stackEqsImpliesStackEq % at k))) >>= \case
        Just v -> return $ nameS v
        Nothing -> do
            addr <- addVarM "stack-eq-witness" word32T
            assertFactSmtM (eqS (bvandS (nameS addr) (hexS "00000003")) (hexS "00000000"))
            sp_smt <- smtExprNoSplitM sp
            assertFactSmtM (bvuleS sp_smt (nameS addr))
            let ptr = smtExprE word32T (SMT (nameS addr))
            let eq = eqE (memAccE word32T ptr s1) (memAccE word32T ptr s2)
            stack_eq <- addDefNoSplitM "stack-eq" eq
            liftSolver $ #stackEqsImpliesStackEq %= M.insert k stack_eq
            return (nameS stack_eq)

getStackEqImplies :: MonadSolver m => S -> S -> SMT -> ReaderT SMTEnv m S
getStackEqImplies split st_top other = do
    let (rhs, cond) = case other of
            SMTSplitMem splitMem -> (splitMem.top, bvuleS splitMem.split split)
            SMT other' -> (other', trueS)
    noteModelExprM (eqS st_top rhs) boolT
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

addSplitMemVarM :: MonadSolver m => S -> NameHint -> ExprType -> m SplitMem
addSplitMemVarM addr nm ty@ExprTypeMem = do
    bot_mem <- addVarM (nm ++ "_bot") ty
    top_mem <- addVarM (nm ++ "_top") ty
    liftSolver $ #stackEqsStackEqImpliesCheck %= M.insert (nameS top_mem) Nothing
    return $ SplitMem
        { split = addr
        , top = nameS top_mem
        , bottom = nameS bot_mem
        }

-- TODO
addPValidDomAssertionsM :: MonadSolver m => m ()
addPValidDomAssertionsM = do
    ensureM cheatMemDoms
    return ()

mergeEnvs :: MonadSolver m => [(Expr, SMTEnv)] -> m SMTEnv
mergeEnvs envs = do
    var_envs' <- fmap join $ for envs $ \(pc, env) -> do
        pc_str <- withEnv env $ smtExprM pc
        return $
            [ M.singleton var (M.singleton s ([pc_str] :: [SMT]))
            | (var, s) <- M.toAscList env
            ]
    let var_envs = foldr (M.unionWith (M.unionWith (<>))) M.empty var_envs'
    let f :: [(SMT, [SMT])] -> SMT
        f itsx =
            let Just (its, (v', _)) = unsnoc itsx
                g v (v2, pc_strs'') =
                    let pc_strs = fmap (^. expecting #_SMT) pc_strs''
                        pc_str = case pc_strs of
                            [pc_str'] -> pc_str'
                            _ -> orNS pc_strs
                     in smtIfThenElse pc_str v2 v
             in foldl g v' its
    return $ fmap (f . sortOn (smtComparisonKey . fst) . M.toAscList) var_envs

data SMTComparisonKey
  = SMTComparisonKeySMT String
  | SMTComparisonKeySplitMem String String String
  deriving (Eq, Generic, Ord, Show)

smtComparisonKey :: SMT -> SMTComparisonKey
smtComparisonKey = \case
    SMT s -> SMTComparisonKeySMT $ showSExprWithPlaceholders s
    SMTSplitMem s -> SMTComparisonKeySplitMem (showSExprWithPlaceholders s.split) (showSExprWithPlaceholders s.top) (showSExprWithPlaceholders s.bottom)

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

mergeEnvsPcs :: MonadSolver m => [(Expr, SMTEnv)] -> m (Expr, SMTEnv, Bool)
mergeEnvsPcs pc_envs' = do
    let pc_envs = flip filter pc_envs' $ \(pc, _) -> pc /= falseE
    let path_cond = case pc_envs of
            [] -> falseE
            _ ->
                let pcs = nub $ map fst pc_envs
                 in foldAssocBalanced orE pcs
    env <- mergeEnvs pc_envs
    return $ (path_cond, env, length pc_envs > 1)
