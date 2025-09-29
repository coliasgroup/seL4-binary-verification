{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module BV.Core.GraphSlice.Old.Solver
    ( ExprEnv
    , Name (..)
      -- , NameHint
    , GraphSliceSolverT
    , PcEnv (..)
    , SolverExpr
    , SolverExprContext (..)
    , addDef
    , addPValidDomAssertions'
    , addSplitMemVar
    , addVar
    , assertFact
    , convertExpr'
    , convertExprNotSplit
    , convertInnerExpr
    , getImmBasisMems
    , getModelExprs
    , getModelVars
    , mergeEnvsPcs
    , nameS
    , runGraphSliceSolverTStep
    , tryGetDef
    , withEnv
    , withoutEnv
    ) where

import BV.Core.GraphSlice.New.Common
import BV.Core.GraphSlice.New.SendSolverExprCommand (SolverExpr,
                                                     SolverExprContext (..))

import BV.Core.GenerateFreshName
import BV.Core.Logic
import BV.Core.Structs
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils (whenNothing, withMapSlotWith)
import BV.SMTLIB2.SExpr
import BV.Utils

import Control.DeepSeq (NFData)
import Control.Monad (unless, when, (>=>))
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (Reader, ReaderT (runReaderT), asks)
import Control.Monad.RWS (lift, modify)
import Control.Monad.State (StateT, evalStateT, get, mapStateT)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Reader (mapReaderT)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.List (nub, sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString (..))
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%%=), (%=), (<<%=))
import Text.Printf (printf)

-- TODO
cheatMemDoms :: Bool
cheatMemDoms = True

--

type T = GraphSliceSolverT

type C = MonadGraphSliceSendSExpr

--

newtype GraphSliceSolverT m a
  = GraphSliceSolverT { run :: StateT TState (ReaderT TEnv (StructsT m)) a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance MonadTrans GraphSliceSolverT where
    lift = GraphSliceSolverT . lift . lift . lift

liftStructs :: Monad m => StructsT m a -> T m a
liftStructs = GraphSliceSolverT . lift . lift

liftPure :: Monad m => StateT TState (Reader TEnv) a -> T m a
liftPure = GraphSliceSolverT . mapStateT (mapReaderT (return . runIdentity))

liftSolver :: Monad m => StateT TState (Reader TEnv) a -> T m a
liftSolver = liftPure

send :: C m => SExprWithPlaceholders -> T m ()
send = lift . sendSExpr

runGraphSliceSolverTStep :: C m => (Ident -> Struct) -> ROData -> T m a -> m a
runGraphSliceSolverTStep lookupStruct rodata m =
      runStructsT lookupStruct
    . flip runReaderT (initEnv rodata)
    . flip evalStateT initState
    . (.run)
    $ m'
  where
    m' = do
        initSolver
        m

data TEnv
  = TEnv
      { rodata :: ROData
      }
  deriving (Generic)

data TState
  = TState
      { namesUsed :: Set Name
      , externalNames :: Set Name
      , pvalids :: Map S (Map PValidKey S)
      , ptrs :: Map S Name
      , cachedExprs :: Map S Name
      , cachedExprNames :: Set Name
      , defs :: Map Name S
      , doms :: Set (S, S, S)
      , modelVars :: Set Name
      , modelExprs :: Map SExprWithPlaceholders (Name, ExprType)
      , stackEqImpliesCheckMap :: Map Name (Maybe S)
      , impliesStackEqMap :: Map (GraphExpr, GraphExpr, GraphExpr) Name
      , tokens :: Map Ident Name
      , tokenVals :: Map S Ident
      , smtDerivedOps :: Map (Op, Integer) String
      }
  deriving (Generic)

initEnv :: ROData -> TEnv
initEnv rodata = TEnv
    { rodata
    }

initState :: TState
initState = TState
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
    , stackEqImpliesCheckMap = M.empty
    , impliesStackEqMap = M.empty
    , tokens = M.empty
    , tokenVals = M.empty
    , smtDerivedOps = M.empty
    }

initSolver :: C m => T m ()
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

takeFreshName :: C m => NameHint -> T m Name
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

withMapSlot :: (C m, Ord k) => Lens' TState (M.Map k v) -> k -> T m v -> T m v
withMapSlot = withMapSlotWith $ liftSolver . mapStateT (return . runIdentity)

withMapSlotR :: (C m, Ord k) => Lens' TState (M.Map k v) -> k -> ReaderT r (T m) v -> ReaderT r (T m) v
withMapSlotR l k = mapReaderT $ withMapSlotWith (liftSolver . mapStateT (return . runIdentity)) l k

--

getModelVars :: C m => T m (Set Name)
getModelVars = liftSolver $ use #modelVars

getModelExprs :: C m => T m (Map SExprWithPlaceholders (Name, ExprType))
getModelExprs = liftSolver $ use #modelExprs

askRODataPtrs :: C m => T m [(Expr c, ExprType)]
askRODataPtrs = do
    rodata <- liftSolver $ gview #rodata
    return
        [ (machineWordE range.addr, structT structName)
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
    OpWordArrayAccess -> "select"
    OpWordArrayUpdate -> "store"
    OpExt OpExtROData -> "rodata"
    OpExt OpExtImpliesROData -> "implies-rodata"

--

getDef :: C m => Name -> T m S
getDef name = fromJust <$> tryGetDef name

tryGetDef :: C m => Name -> T m (Maybe S)
tryGetDef name = liftSolver $ use $ #defs % at name

addDef :: C m => NameHint -> GraphExpr -> ReaderT ExprEnv (T m) MaybeSplit
addDef nameHint val =
    either (NotSplit . nameS) Split <$> addDefInner nameHint val

addDefNotSplit :: C m => NameHint -> GraphExpr -> ReaderT ExprEnv (T m) Name
addDefNotSplit nameHint val =
    viewExpecting #_Left <$> addDefInner nameHint val

addDefInner :: C m => NameHint -> GraphExpr -> ReaderT ExprEnv (T m) (Either Name SplitMem)
addDefInner nameHint expr = convertExpr' expr >>= \case
    NotSplit s -> lift $ Left <$> do
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
        let add suffix ty' s =
                nameS <$> addDefNotSplit (nameHint ++ "_" ++ suffix) (smtExprE ty' (NotSplit s))
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

addVar :: C m => NameHint -> ExprType -> T m Name
addVar nameHint ty = do
    name <- takeFreshName nameHint
    unless (isTypeOmitted ty) $ do
        send $ declareFunS name.unwrap [] (typeToSMT ty)
        when (isTypeRepresentable ty) $ do
            liftSolver $ #modelVars %= S.insert name
    return name

assertSMTFact :: C m => S -> T m ()
assertSMTFact = send . assertS

assertFact :: C m => GraphExpr -> ReaderT ExprEnv (T m) ()
assertFact = convertExprNotSplit >=> lift . assertSMTFact

noteModelExpr :: C m => S -> ExprType -> T m ()
noteModelExpr s ty = void $ withMapSlot #modelExprs s $ do
    let sanitized = take 20 (filter (`notElem` (" ()" :: String)) (showSExprWithPlaceholders s))
    v <- withoutEnv $ addDefNotSplit ("query_" ++ sanitized) (smtExprE ty (NotSplit s))
    return (v, ty)

maybeNoteModelExpr :: C m => S -> ExprType -> [GraphExpr] -> T m ()
maybeNoteModelExpr s ty subexprs =
    when (isTypeRepresentable ty && not (all (isTypeRepresentable . (.ty)) subexprs)) $ do
        noteModelExpr s ty

notePtr :: C m => S -> T m Name
notePtr p = withMapSlot #ptrs p $
    withoutEnv $ addDefNotSplit "ptr" (smtExprE machineWordT (NotSplit p))

noteMemDom :: C m => S -> S -> S -> T m ()
noteMemDom p d md = liftSolver $ #doms %= S.insert (p, d, md)

cacheLargeExpr :: C m => S -> NameHint -> ExprType -> T m S
cacheLargeExpr s nameHint ty = do
    nameOpt <- liftSolver $ use $ #cachedExprs % at s
    case nameOpt of
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

getToken :: C m => Ident -> T m MaybeSplit
getToken ident = fmap (NotSplit . nameS) $ withMapSlot #tokens ident $ do
    n <- liftSolver $ (+)
        <$> use (#tokens % to M.size)
        <*> use (#tokenVals % to M.size)
    k <- withoutEnv $
        addDefNotSplit
            ("token_" ++ ident.unwrap)
            (numE compiledTokenType (toInteger (n + 1)))
    v <- getDef k
    liftSolver $ #tokenVals %= M.insert v ident
    return k

compiledTokenType :: ExprType
compiledTokenType = wordT 64

--

addRODataDef :: C m => T m ()
addRODataDef = do
    roName <- takeFreshName "rodata"
    impRoName <- takeFreshName "implies-rodata"
    ensureM $ roName == "rodata"
    ensureM $ impRoName == "implies-rodata"
    rodataPtrs <- askRODataPtrs
    let memParamName = "m"
    (roDef, impRoDef) <- case rodataPtrs of
        [] -> do
            return (trueS, trueS)
        _ -> do
            roWitness <- addVar "rodata-witness" word32T
            roWitnessVal <- addVar "rodata-witness-val" word32T
            ensureM $ roWitness == "rodata-witness"
            ensureM $ roWitnessVal == "rodata-witness-val"
            let roWitnessS = nameS roWitness
            let roWitnessValS = nameS roWitnessVal
            rodata <- liftSolver $ gview #rodata
            let eqs =
                    [ eqS ["load-word32", symbolS memParamName, p] v
                    | (p, v) <-
                        [ (machineWordS p, machineWordS v)
                        | (p, v) <- M.toList rodata.rodata
                        ] ++
                        [ (roWitnessS, roWitnessValS)
                        ]
                    ]
            assertSMTFact $ orNS
                [ andS
                    (machineWordS range.addr `bvuleS` roWitnessS)
                    (roWitnessS `bvuleS` machineWordS (range.addr + range.size - 1))
                | range <- rodata.ranges
                ]
            assertSMTFact $ eqS
                (roWitnessS `bvandS` machineWordS 3)
                (machineWordS 0)
            return (andNS eqs, last eqs)
    send $ defineFunS
        roName.unwrap
        [(memParamName, typeToSMT ExprTypeMem)]
        boolS
        roDef
    send $ defineFunS
        impRoName.unwrap
        [(memParamName, typeToSMT ExprTypeMem)]
        boolS
        impRoDef

-- TODO
addPValidDomAssertions' :: C m => T m ()
addPValidDomAssertions' = do
    ensureM cheatMemDoms
    return ()

--

addSplitMemVar :: C m => S -> NameHint -> ExprType -> T m SplitMem
addSplitMemVar split nameHint ty@ExprTypeMem = do
    bottom <- addVar (nameHint ++ "_bot") ty
    top <- addVar (nameHint ++ "_top") ty
    liftSolver $ #stackEqImpliesCheckMap %= M.insert top Nothing
    return $ SplitMem
        { split
        , top = nameS top
        , bottom = nameS bottom
        }

--

data PcEnv
  = PcEnv
      { pc :: GraphExpr
      , env :: ExprEnv
      }
  deriving (Eq, Generic, NFData, Ord, Show)

mergeEnvsPcs :: C m => [PcEnv] -> T m (PcEnv, Bool)
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

mergeEnvs :: C m => [PcEnv] -> T m ExprEnv
mergeEnvs envs = do
    varValPcList <- fmap concat $ for envs $ \(PcEnv pc env) -> do
        pc' <- withEnv env $ convertExprNotSplit pc
        return
            [ (var, val, pc')
            | (var, val) <- M.toList env
            ]
    let varValPcMap = foldr (M.unionWith (M.unionWith (<>))) M.empty $
            [ M.singleton var (M.singleton val [pc'])
            | (var, val, pc') <- varValPcList
            ]
    return $ mergeValPcMapCompat <$> varValPcMap
  where
    -- HACK impl compatible with graph-refine
    mergeValPcMapCompat = mergeValPcListCompat . sortOn (compatSMTComparisonKey . fst) . M.toList
    mergeValPcListCompat valsByPc =
        let Just (valsByPcInit, (lastVal, _)) = unsnoc valsByPc
            f accVal (val, pcs) = convertIfThenElse (orCompat pcs) val accVal
         in foldl f lastVal valsByPcInit
    orCompat = \case
        [x] -> x
        xs -> orNS xs

data CompatSMTComparisonKey
  = SMTComparisonKeyNotSplit String
  | SMTComparisonKeySplit String String String
  deriving (Eq, Generic, Ord, Show)

compatSMTComparisonKey :: MaybeSplit -> CompatSMTComparisonKey
compatSMTComparisonKey = \case
    NotSplit s -> SMTComparisonKeyNotSplit
        (showSExprWithPlaceholders s)
    Split s -> SMTComparisonKeySplit
        (showSExprWithPlaceholders s.split)
        (showSExprWithPlaceholders s.top)
        (showSExprWithPlaceholders s.bottom)

--

-- TODO rename
convertInnerExpr :: C m => GraphExpr -> ReaderT ExprEnv (T m) SolverExpr
convertInnerExpr expr = case expr.ty of
    ExprTypeRelWrapper -> case expr.value of
        ExprValueOp { } -> traverseOf (exprArgs % traversed) convertInnerExpr expr
    _ -> smtExprE expr.ty <$> convertExpr' expr

convertExprNotSplit :: C m => GraphExpr -> ReaderT ExprEnv (T m) S
convertExprNotSplit expr = viewExpecting #_NotSplit <$> convertExpr' expr

convertExpr' :: C m => GraphExpr -> ReaderT ExprEnv (T m) MaybeSplit
convertExpr' expr = case expr.value of
    ExprValueOp op args -> case op of
        _ | op == OpWordCast || op == OpWordCastSigned -> do
                let [v] = args
                let ExprTypeWord exprBits = expr.ty
                let ExprTypeWord vBits = v.ty
                v' <- convertExprNotSplit v
                return $ NotSplit $ if
                    | exprBits == vBits -> v'
                    | exprBits < vBits -> [ixS "extract" [intS (exprBits - 1), intS (0 :: Integer)], v']
                    | otherwise -> case op of
                        OpWordCast -> [ixS "zero_extend" [intS (exprBits - vBits)], v']
                        OpWordCastSigned -> [ixS "sign_extend" [intS (exprBits - vBits)], v']
        _ | op == OpCountLeadingZeroes || op == OpWordReverse -> do
                let [v] = args
                let ExprTypeWord bits = expr.ty
                v' <- convertExprNotSplit v
                op' <- lift $ getDerivedOp op bits
                return $ NotSplit [op', v']
        OpCountTrailingZeroes -> do
                let [v] = args
                convertExpr' $ clzE (wordReverseE v)
        _ | op `elem` ([OpPValid, OpPGlobalValid, OpPWeakValid, OpPArrayValid] :: [Op]) -> do
                (htd, tyExpr, p, mkPvTy) <- case op of
                    OpPArrayValid -> do
                        let [htd, tyExpr, p, len] = args
                        len' <- convertInnerExpr len
                        let mkPvTy ty = PValidTypeArray { ty, len = len' }
                        return (htd, tyExpr, p, mkPvTy)
                    _ -> do
                        let [htd, tyExpr, p] = args
                        let mkPvTy = PValidTypeType
                        return (htd, tyExpr, p, mkPvTy)
                let ExprValueType ty = tyExpr.value
                htd' <- gview $ expectingAt (nameTyFromVarE htd) % expecting #_NotSplit
                p' <- convertExprNotSplit p
                lift $ NotSplit <$> addPValids htd' (pvalidKindFromOp op) (mkPvTy ty) p'
        OpMemDom -> do
                let [p, dom] = args
                p' <- convertExprNotSplit p
                dom' <- convertExprNotSplit dom
                let md = [opS op, p', dom']
                lift $ noteMemDom p' dom' md
                return $ NotSplit $ if cheatMemDoms then trueS else md
        OpMemUpdate -> do
                let [m, p, v] = args
                ensureM $ isWordT v.ty
                m' <- convertExpr' m
                p' <- convertExprNotSplit p
                v' <- convertExprNotSplit v
                lift $ convertMemUpdate m' p' v' v.ty
        OpMemAcc -> do
                let [m, p] = args
                ensureM $ isWordT expr.ty
                m' <- convertExpr' m
                p' <- convertExprNotSplit p
                lift $ NotSplit <$> convertMemAccess m' p' expr.ty
        OpExt OpExtStackEqualsImplies -> do
                args' <- traverse convertExpr' args
                let [NotSplit sp1, stack1, NotSplit sp2, stack2] = args'
                if sp1 == sp2 && stack1 == stack2
                    then return $ NotSplit trueS
                    else do
                        eq <- lift $ getStackEqImplies (viewExpecting #_Split stack2) stack1
                        return $ NotSplit $ (sp1 `eqS` sp2) `andS` eq
        OpExt OpExtImpliesStackEquals -> do
                let [sp1, stack1, sp2, stack2] = args
                eq <- addImpliesStackEq sp1 stack1 stack2
                sp1' <- convertExprNotSplit sp1
                sp2' <- convertExprNotSplit sp2
                return $ NotSplit $ (sp1' `eqS` sp2') `andS` eq
        OpIfThenElse -> do
                let [cond, x, y] = args
                convertIfThenElse
                    <$> convertExprNotSplit cond
                    <*> convertExpr' x
                    <*> convertExpr' y
        OpHtdUpdate -> do
                lift $ NotSplit . nameS <$> addVar "update_htd" expr.ty
        OpEquals | (head args).ty == ExprTypeMem -> do
                args' <- traverse convertExprNotSplit args
                let [x, y] = args'
                let s = ["mem-eq", x, y]
                lift $ noteModelExpr s boolT
                return $ NotSplit s
        OpEquals | (head args).ty == word32T -> do
                args' <- traverse convertExprNotSplit args
                let [x, y] = args'
                let s = ["word32-eq", x, y]
                return $ NotSplit s
        _ -> do
                args' <- traverse convertExprNotSplit args
                let op' = opS op
                let s = case args' of
                        [] -> op'
                        _ -> List $ [op'] ++ args'
                lift $ maybeNoteModelExpr s expr.ty args
                return $ NotSplit s
    ExprValueNum n -> do
        return $ NotSplit $ intWithWidthS (wordTBits expr.ty) n
    ExprValueVar var -> do
        let key = NameTy var expr.ty
        let err = error $ "env miss: " ++ show key
        asks $ M.findWithDefault err key
    ExprValueToken tok -> do
        lift $ getToken tok
    ExprValueSMTExpr s -> do
        return s

convertIfThenElse :: S -> MaybeSplit -> MaybeSplit -> MaybeSplit
convertIfThenElse cond x y = case (x, y) of
    (NotSplit xns, NotSplit yns) -> NotSplit $ iteS cond xns yns
    _ ->
        let xs = trivSplit x
            ys = trivSplit y
         in Split $ SplitMem
                { split =
                    if xs.split == ys.split
                    then xs.split
                    else iteS cond xs.split ys.split
                , top = iteS cond xs.top ys.top
                , bottom = iteS cond xs.bottom ys.bottom
                }
  where
    trivSplit = \case
        Split splitMem -> splitMem
        NotSplit s -> SplitMem
            { split = machineWordS 0
            , top = s
            , bottom = s
            }

getDerivedOp :: C m => Op -> Integer -> T m S
getDerivedOp op bits = fmap symbolS $ withMapSlot #smtDerivedOps (op, bits) $ do
    let fname = case op of
            OpCountLeadingZeroes -> printf "bvclz_%d" bits
            OpWordReverse -> printf "bvrev_%d" bits
    body <- case bits of
        1 -> return $ case op of
            OpCountLeadingZeroes -> iteS ("x" `eqS` binS "0") (binS "1") (binS "0")
            OpWordReverse -> "x"
        _ -> do
            let botBits = bits `div` 2
            let topBits = bits - botBits
            topOp <- getDerivedOp op topBits
            botOp <- getDerivedOp op botBits
            let top = [ixS "extract" [intS (bits - 1), intS botBits], "x"]
                bot = [ixS "extract" [intS (botBits - 1), intS (0 :: Integer)], "x"]
                topApp = [topOp, top]
                botApp = [botOp, bot]
                topAppExtended = [ixS "zero_extend" [intS botBits], topApp]
                botAppExtended = [ixS "zero_extend" [intS topBits], botApp]
            return $ case op of
                OpCountLeadingZeroes ->
                    iteS
                        (top `eqS` intWithWidthS topBits 0)
                        (bvaddS botAppExtended (intWithWidthS bits botBits))
                        topAppExtended
                OpWordReverse ->
                    concatS botApp topApp
    send $ defineFunS fname [("x", bitVecS bits)] (bitVecS bits) body
    return fname

convertMemUpdate :: C m => MaybeSplit -> S -> S -> ExprType -> T m MaybeSplit
convertMemUpdate memMaybeSplit p v ty@(ExprTypeWord bits) = case memMaybeSplit of
    Split mem -> Split <$> do
        p' <- cacheLargeExpr p "memupd_pointer" word32T
        v' <- cacheLargeExpr v "memupd_val" ty
        top <- cacheLargeExpr mem.top "split_mem_top" memT
        topUpd <- fromNotSplit <$> convertMemUpdate (NotSplit top) p' v' ty
        bottom <- cacheLargeExpr mem.bottom "split_mem_bot" memT
        bottomUpd <- fromNotSplit <$> convertMemUpdate (NotSplit bottom) p' v' ty
        let f = iteS (mem.split `bvuleS` p')
        return $ SplitMem
            { split = mem.split
            , top = f topUpd top
            , bottom = f bottom bottomUpd
            }
    NotSplit mem -> NotSplit <$> case bits of
        8 -> do
            p' <- cacheLargeExpr p "memupd_pointer" word32T
            let align = p' `bvandS` hexS "fffffffd"
            noteModelExpr align word32T
            noteModelExpr (loadWord32S mem align) word32T
            return $ storeWord8S mem p' v
        _ -> do
            let (load, store) = case bits of
                    32 -> (loadWord32S, storeWord32S)
                    64 -> (loadWord64S, storeWord64S)
            noteModelExpr (load mem p) ty
            noteModelExpr p word32T
            return $ store mem p v

convertMemAccess :: C m => MaybeSplit -> S -> ExprType -> T m S
convertMemAccess memMaybeSplit p ty@(ExprTypeWord bits) = case memMaybeSplit of
    Split mem -> do
        p' <- cacheLargeExpr p "memacc_pointer" word32T
        let f side = convertMemAccess (NotSplit (side mem)) p' ty
        iteS (bvuleS mem.split p') <$> f (.top) <*> f (.bottom)
    NotSplit mem -> do
        let load = case bits of
                8 -> loadWord8S
                32 -> loadWord32S
                64 -> loadWord64S
        let v = load mem p
        noteModelExpr p word32T
        noteModelExpr v ty
        return v

addImpliesStackEq :: C m => GraphExpr -> GraphExpr -> GraphExpr -> ReaderT ExprEnv (T m) S
addImpliesStackEq sp stack1 stack2 = fmap nameS $ withMapSlotR #impliesStackEqMap (sp, stack1, stack2) $ do
    addr <- lift $ nameS <$> addVar "stack-eq-witness" word32T
    lift $ assertSMTFact $ (addr `bvandS` hexS "00000003") `eqS` hexS "00000000"
    sp' <- convertExprNotSplit sp
    lift $ assertSMTFact $ sp' `bvuleS` addr
    let f = memAccE word32T (smtExprE word32T (NotSplit addr))
    addDefNotSplit "stack-eq" $ f stack1 `eqE` f stack2

getStackEqImplies :: C m => SplitMem -> MaybeSplit -> T m S
getStackEqImplies stack1 stack2 = do
    let (rhs, cond) = case stack2 of
            Split (SplitMem { split, top }) -> (top, split `bvuleS` stack1.split)
            NotSplit s -> (s, trueS)
    noteModelExpr (stack1.top `eqS` rhs) boolT
    mems <- getImmBasisMems stack1.top
    let [k] = S.toList mems
    oldOpt <- liftSolver $ use $ #stackEqImpliesCheckMap % expectingAt k
    case oldOpt of
        Nothing -> liftSolver $ #stackEqImpliesCheckMap %= M.insert k (Just rhs)
        Just old -> ensureM $ old == rhs
    return $ cond `impliesS` (stack1.top `eqS` rhs)

getImmBasisMems :: C m => S -> T m (Set Name)
getImmBasisMems = go
  where
    go = \case
        List [op, x, _, _] | isStore op ->
            go x
        List [op, _, l, r] | op == symbolS "ite" ->
                (<>) <$> go l <*> go r
        m -> do
            let Just sym = parseSymbolS m
            tryGetDef (Name sym) >>= \case
                Just x -> go x
                Nothing -> return $ S.singleton (Name sym)
    isStore s = s `elem`
        ([ symbolS "store-word8"
         , symbolS "store-word32"
         , symbolS "store-word64"
         ] :: [SExprWithPlaceholders])

data PValidKey
  = PValidKey
      { pvKind :: PValidKind
      , pvTy :: PValidType SolverExprContext
      , ptrName :: Name
      }
  deriving (Eq, Generic, NFData, Ord, Show)

addPValids :: C m => S -> PValidKind -> PValidType SolverExprContext -> S -> T m S
addPValids = go
  where
    go htd pvKind pvTy ptr = case htd of
        List [op, cond, l, r] | op == symbolS "ite" ->
            iteS cond
                <$> go l pvKind pvTy ptr
                <*> go r pvKind pvTy ptr
        _ -> do
            new <- liftSolver $ (#pvalids % at htd) %%= \slot ->
                (isNothing slot, Just (fromMaybe M.empty slot))
            when new $ do
                rodataPtrs <- askRODataPtrs
                for_ rodataPtrs $ \(roAddr, roTy) -> do
                    roAddr' <- withoutEnv $ convertExprNotSplit roAddr
                    assertSMTFact =<<
                        goAssumingAlreadyExists htd PValidKindPGlobalValid (PValidTypeType roTy) roAddr'
            goAssumingAlreadyExists htd pvKind pvTy ptr
    goAssumingAlreadyExists htd pvKind pvTy ptr = do
        ptrName <- notePtr ptr
        let key = PValidKey { pvKind, pvTy, ptrName }
        opt <- liftSolver $ use $ #pvalids % expectingAt htd % at key
        whenNothing opt $ do
            var <- nameS <$> addVar "pvalid" boolT
            let info = mkPvInfo key var
            do
                fact <- liftStructs $ alignValidIneq info.pvTy info.p
                withoutEnv $ assertFact $ castExpr $ info.pv `impliesE` fact
            others <- liftSolver $ #pvalids % expectingAt htd <<%= M.insert key var
            for_ (sortOthersCompat others) $ \(otherKey, otherVar) -> do
                let otherInfo = mkPvInfo otherKey otherVar
                let pvKinds :: [PValidKind] = [otherInfo.pvKind, info.pvKind]
                unless (PValidKindPWeakValid `elem` pvKinds && PValidKindPGlobalValid `notElem` pvKinds) $ do
                    let applyAssertion f = do
                            fact <- liftStructs $ f info otherInfo
                            withoutEnv $ assertFact $ castExpr fact
                    applyAssertion pvalidAssertion1
                    applyAssertion pvalidAssertion2
            return var
    mkPvInfo (PValidKey { pvKind, pvTy, ptrName }) var = PValidInfo
        { pvKind
        , pvTy
        , p = smtExprE machineWordT (NotSplit (nameS ptrName))
        , pv = smtExprE boolT (NotSplit var)
        }
    -- HACK matches graph-refine
    sortOthersCompat = sortOn snd . M.toList
