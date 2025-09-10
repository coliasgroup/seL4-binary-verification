{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Experimental.Flatten
    ( FlattenState
    , MonadRepGraphFlatten (..)
    , MonadRepGraphFlattenSend (..)
    , addSplitMemVar
    , addVar
    , flattenAndAddDef
    , flattenAndAssertFact
    , flattenExpr
    , getModelExprs
    , getModelVars
    , initFlattenEnv
    , initFlattenState
    , mergeEnvsPcs
    , tryGetDef
    , withEnv
    , withoutEnv
    ) where

import BV.Core.Experimental.Types

import BV.Core.GenerateFreshName (generateFreshName)
import BV.Core.Logic
import BV.Core.Structs (MonadStructs)
import BV.Core.Types hiding (SplitMem (..))
import BV.Core.Types.Extras
import BV.Core.Utils
import BV.Utils

import Control.DeepSeq (NFData)
import Control.Monad (unless, when, (>=>))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (Reader, ReaderT (runReaderT), asks)
import Control.Monad.RWS (MonadState (get), MonadWriter (..), RWST)
import Control.Monad.State (StateT, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Writer (WriterT)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Functor (void)
import Data.List (nub, sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%%=), (%=), (<<%=))

class Monad m => MonadRepGraphFlattenSend m where
    sendCommand :: Command -> m ()

instance Monad m => MonadRepGraphFlattenSend (WriterT [Command] m) where
    sendCommand s = tell [s]

class (MonadStructs m, MonadRepGraphFlattenSend m) => MonadRepGraphFlatten m where
    liftFlatten :: StateT FlattenState (Reader FlattenEnv) a -> m a

instance (Monoid w, MonadRepGraphFlatten m) => MonadRepGraphFlatten (RWST r w s m) where
    liftFlatten = lift . liftFlatten

instance (Monoid w, MonadRepGraphFlattenSend m) => MonadRepGraphFlattenSend (RWST r w s m) where
    sendCommand = lift . sendCommand

instance MonadRepGraphFlatten m => MonadRepGraphFlatten (ReaderT r m) where
    liftFlatten = lift . liftFlatten

instance MonadRepGraphFlattenSend m => MonadRepGraphFlattenSend (ReaderT r m) where
    sendCommand = lift . sendCommand

instance MonadRepGraphFlatten m => MonadRepGraphFlatten (StateT s m) where
    liftFlatten = lift . liftFlatten

instance MonadRepGraphFlattenSend m => MonadRepGraphFlattenSend (StateT s m) where
    sendCommand = lift . sendCommand

instance MonadRepGraphFlatten m => MonadRepGraphFlatten (MaybeT m) where
    liftFlatten = lift . liftFlatten

instance MonadRepGraphFlattenSend m => MonadRepGraphFlattenSend (MaybeT m) where
    sendCommand = lift . sendCommand

instance MonadRepGraphFlatten m => MonadRepGraphFlatten (ExceptT e m) where
    liftFlatten = lift . liftFlatten

instance MonadRepGraphFlattenSend m => MonadRepGraphFlattenSend (ExceptT e m) where
    sendCommand = lift . sendCommand

data FlattenEnv
  = FlattenEnv
      { rodataPtrs :: [Expr]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data FlattenState
  = FlattenState
      { names :: Set Ident
      , defs :: Map Ident Expr
      , cachedExprs :: Map Expr Ident
      , cachedImpliesStackEqs :: Map (Expr, Expr, Expr) Ident
      , tokens :: Map Ident Ident
      , pvalids :: Map Ident (Map PValidKey Expr)
      , modelVars :: Set Ident
      , modelExprs :: Map Expr Ident
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PValidKey
  = PValidKey
      { pvKind :: PValidKind
      , pvTy :: PValidType
      , ptr :: NameTy
      }
  deriving (Eq, Generic, NFData, Ord, Show)

initFlattenEnv :: [Expr] -> FlattenEnv
initFlattenEnv rodataPtrs = FlattenEnv
    { rodataPtrs
    }

initFlattenState :: FlattenState
initFlattenState = FlattenState
    { names = S.empty
    , defs = M.empty
    , cachedExprs = M.empty
    , cachedImpliesStackEqs = M.empty
    , tokens = M.empty
    , pvalids = M.empty
    , modelVars = S.empty
    , modelExprs = M.empty
    }

--

send :: MonadRepGraphFlatten m => Command -> m ()
send = sendCommand

--

type ExprEnv = Map NameTy Expr

withEnv :: ExprEnv -> ReaderT ExprEnv m a -> m a
withEnv = flip runReaderT

withoutEnv :: ReaderT ExprEnv m a -> m a
withoutEnv = flip runReaderT mempty

--

withMapSlot :: (MonadRepGraphFlatten m, Ord k) => Lens' FlattenState (M.Map k v) -> k -> m v -> m v
withMapSlot l k m = do
    opt <- liftFlatten (use (l % at k))
    whenNothing opt $ do
        v <- m
        liftFlatten $ l %= M.insert k v
        return v

--

askRODataPtrs :: MonadRepGraphFlatten m => m [Expr]
askRODataPtrs = liftFlatten $ gview #rodataPtrs

--

getModelVars :: MonadRepGraphFlatten m => m (Set Ident)
getModelVars = liftFlatten $ use #modelVars

getModelExprs :: MonadRepGraphFlatten m => m (Map Expr Ident)
getModelExprs = liftFlatten $ use #modelExprs

--

type NameHint = String

takeFreshName :: MonadRepGraphFlatten m => NameHint -> m Ident
takeFreshName nameHint = liftFlatten $ zoom #names $ do
    names <- get
    let isTaken = flip S.member names . Ident
    let name = Ident (generateFreshName isTaken sanitized)
    modify $ S.insert name
    return name
  where
    sanitized =
        [ if c `elem` ("'#\"" :: String) then '_' else c
        | c <- nameHint
        ]

--

isTypeOmitted :: ExprType -> Bool
isTypeOmitted = \case
    ExprTypeHtd -> True
    ExprTypePms -> True
    _ -> False

isTypeRepresentable :: ExprType -> Bool
isTypeRepresentable = \case
    ExprTypeWord _ -> True
    ExprTypeBool -> True
    ExprTypeToken -> True
    _ -> False

getDef :: MonadRepGraphFlatten m => Ident -> m Expr
getDef name = fromJust <$> tryGetDef name

tryGetDef :: MonadRepGraphFlatten m => Ident -> m (Maybe Expr)
tryGetDef name = liftFlatten $ use $ #defs % at name

addDefWithInline :: MonadRepGraphFlatten m => Maybe InlineHint -> NameHint -> Expr -> m NameTy
addDefWithInline inline nameHint expr = do
    name <- takeFreshName nameHint
    let var = NameTy name expr.ty
    unless (isTypeOmitted expr.ty) $ do
        send $ CommandDefine inline var expr
        liftFlatten $ #defs %= M.insert name expr
        when (isTypeRepresentable expr.ty) $ do
            liftFlatten $ #modelVars %= S.insert name
    return var

addDef :: MonadRepGraphFlatten m => NameHint -> Expr -> m NameTy
addDef = addDefWithInline (Just InlineHintDontInline)

addVar :: MonadRepGraphFlatten m => NameHint -> ExprType -> m NameTy
addVar nameHint ty = do
    name <- takeFreshName nameHint
    let var = NameTy name ty
    unless (isTypeOmitted ty) $ do
        send $ CommandDeclare var
        when (isTypeRepresentable ty) $ do
            liftFlatten $ #modelVars %= S.insert name
    return var

cacheExprWithInline :: MonadRepGraphFlatten m => Maybe InlineHint -> NameHint -> Expr -> m NameTy
cacheExprWithInline inline nameHint expr = do
    name <- withMapSlot #cachedExprs expr $ (.name) <$> addDefWithInline inline nameHint expr
    return $ NameTy name expr.ty

cacheExpr :: MonadRepGraphFlatten m => NameHint -> Expr -> m NameTy
cacheExpr = cacheExprWithInline (Just InlineHintDontInline)

cacheExprInline :: MonadRepGraphFlatten m => NameHint -> Expr -> m NameTy
cacheExprInline = cacheExprWithInline (Just InlineHintInline)

noteModelExpr :: MonadRepGraphFlatten m => Expr -> m ()
noteModelExpr expr = void $ withMapSlot #modelExprs expr $ (.name) <$> addDef "query" expr

maybeNoteModelExpr :: MonadRepGraphFlatten m => Expr -> [Expr] -> m ()
maybeNoteModelExpr expr subexprs =
    when (isTypeRepresentable expr.ty && not (all (isTypeRepresentable . (.ty)) subexprs)) $ do
        noteModelExpr expr

getToken :: MonadRepGraphFlatten m => Ident -> m Expr
getToken ident = fmap (varE compiledTokenType) $ withMapSlot #tokens ident $ do
    n <- liftFlatten $ use $ #tokens % to M.size
    (.name) <$> addDef
        ("token_" ++ ident.unwrap)
        (numE compiledTokenType (toInteger n))

compiledTokenType :: ExprType
compiledTokenType = ExprTypeWord 64

assertFact :: MonadRepGraphFlatten m => Expr -> m ()
assertFact = send . CommandAssert

--

addSplitMemVar :: MonadRepGraphFlatten m => Expr -> NameHint -> m Expr
addSplitMemVar split nameHint = do
    top <- varFromNameTyE <$> addVar (nameHint ++ "_top") memT
    bottom <- varFromNameTyE <$> addVar (nameHint ++ "_bot") memT
    return $ splitMemE split top bottom

--

data PcEnv
  = PcEnv
      { pc :: Expr
      , env :: ExprEnv
      }
  deriving (Eq, Generic, NFData, Ord, Show)

mergeEnvsPcs :: MonadRepGraphFlatten m => [PcEnv] -> m (PcEnv, Bool)
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

mergeEnvs :: MonadRepGraphFlatten m => [PcEnv] -> m ExprEnv
mergeEnvs envs = do
    varValPcList <- fmap concat $ for envs $ \(PcEnv pc env) -> do
        return
            [ (var, val, pc)
            | (var, val) <- M.toList env
            ]
    let varValPcMap = foldr (M.unionWith (M.unionWith (<>))) M.empty $
            [ M.singleton var (M.singleton val [pc'])
            | (var, val, pc') <- varValPcList
            ]
    return $ mergeValPcMapCompat <$> varValPcMap
  where
    mergeValPcMapCompat = mergeValPcListCompat . M.toList
    mergeValPcListCompat valsByPc =
        let Just (valsByPcInit, (lastVal, _)) = unsnoc valsByPc
            f accVal (val, pcs) = flattenIfThenElse (orCompat pcs) val accVal
         in foldl f lastVal valsByPcInit
    orCompat = \case
        [x] -> x
        x:xs -> foldr orE x xs

--

flattenAndAddDef :: MonadRepGraphFlatten m => NameHint -> Expr -> ReaderT ExprEnv m NameTy
flattenAndAddDef nameHint = flattenExpr >=> addDef nameHint

flattenAndAssertFact :: MonadRepGraphFlatten m => Expr -> ReaderT ExprEnv m ()
flattenAndAssertFact = flattenExpr >=> assertFact

--

flattenExpr :: MonadRepGraphFlatten m => Expr -> ReaderT ExprEnv m Expr
flattenExpr = walkExprsM $ switchEnv >=> flattenTopLevelExpr

switchEnv :: Monad m => Expr -> ReaderT ExprEnv m Expr
switchEnv expr = case expr.value of
    ExprValueVar name -> do
        let var = NameTy name expr.ty
        let err = error $ "env miss: " ++ show var
        asks $ M.findWithDefault err var
    _ -> return expr

flattenTopLevelExpr :: MonadRepGraphFlatten m => Expr -> m Expr
flattenTopLevelExpr expr = do
    case expr.value of
        ExprValueOp _ args -> do
            maybeNoteModelExpr expr args
        _ -> do
            return ()
    case expr.value of
        ExprValueToken tok -> do
            getToken tok
        ExprValueOp OpIfThenElse [cond, x, y] -> do
            return $ flattenIfThenElse cond x y
        ExprValueOp OpMemUpdate [m, p, v] -> do
            flattenMemUpdate m p v v.ty
        ExprValueOp OpMemAcc [m, p] -> do
            flattenMemAccess m p expr.ty
        ExprValueOp (OpExt OpExtStackEqualsImplies) [sp1, stack1, sp2, stack2] -> do
            if sp1 == sp2 && stack1 == stack2
            then return trueE
            else do
                eq <- getStackEqImplies stack2 stack1
                return $ (sp1 `eqE` sp2) `andE` eq
        ExprValueOp (OpExt OpExtImpliesStackEquals) [sp1, stack1, sp2, stack2] -> do
            eq <- addImpliesStackEq sp1 stack1 stack2
            return $ (sp1 `eqE` sp2) `andE` eq
        ExprValueOp op args | op `elem` [OpPValid, OpPGlobalValid, OpPWeakValid, OpPArrayValid] -> do
            (htd, tyExpr, p, mkPvTy) <- case op of
                OpPArrayValid -> do
                    let [htd, tyExpr, p, len] = args
                    let mkPvTy ty = PValidTypeArray { ty, len }
                    return (htd, tyExpr, p, mkPvTy)
                _ -> do
                    let [htd, tyExpr, p] = args
                    let mkPvTy = PValidTypeType
                    return (htd, tyExpr, p, mkPvTy)
            let ExprValueType ty = tyExpr.value
            addPValids htd (pvalidKindFromOp op) (mkPvTy ty) p
        _ -> do
            return expr

flattenIfThenElse :: Expr -> Expr -> Expr -> Expr
flattenIfThenElse cond x y = case (tryDestructSplitMem x, tryDestructSplitMem y) of
    (NotSplitMem xns, NotSplitMem yns) -> ite xns yns
    (xms, yms) ->
        let xs = trivSplit xms
            ys = trivSplit yms
            split =
                if xs.split == ys.split
                then xs.split
                else (ite `on` (.split)) xs ys
            top = (ite `on` (.top)) xs ys
            bottom = (ite `on` (.bottom)) xs ys
         in splitMemE split top bottom
  where
    ite = ifThenElseE cond
    trivSplit = \case
        SplitMem splitMem -> splitMem
        NotSplitMem expr -> DestructSplitMem
            { split = machineWordE 0
            , top = expr
            , bottom = expr
            }

flattenMemUpdate :: MonadRepGraphFlatten m => Expr -> Expr -> Expr -> ExprType -> m Expr
flattenMemUpdate mem p v ty@(ExprTypeWord bits) = case tryDestructSplitMem mem of
    SplitMem splitMem -> do
        p' <- varFromNameTyE <$> cacheExprInline "memupd_pointer" p
        v' <- varFromNameTyE <$> cacheExprInline "memupd_val" v
        top <- varFromNameTyE <$> cacheExprInline "split_mem_top" splitMem.top
        bottom <- varFromNameTyE <$> cacheExprInline "split_mem_bot" splitMem.bottom
        topUpd <- flattenMemUpdate top p' v' ty
        bottomUpd <- flattenMemUpdate bottom p' v' ty
        let f = ifThenElseE (splitMem.split `lessEqE` p')
        return $ splitMemE splitMem.split (f topUpd top) (f bottom bottomUpd)
    NotSplitMem _ -> case bits of
        8 -> do
            p' <- varFromNameTyE <$> cacheExprInline "memupd_pointer" p
            let aligned = p' `bitwiseAndE` word32E 0xfffffffd
            noteModelExpr aligned
            noteModelExpr $ memAccE word32T aligned mem
            return $ memUpdE p' mem v
        _ -> do
            noteModelExpr p
            noteModelExpr $ memAccE ty p mem
            return $ memUpdE p mem v

flattenMemAccess :: MonadRepGraphFlatten m => Expr -> Expr -> ExprType -> m Expr
flattenMemAccess mem p ty@(ExprTypeWord _) = case tryDestructSplitMem mem of
    SplitMem splitMem -> do
        p' <- varFromNameTyE <$> cacheExprInline "memacc_pointer" p
        let f side = flattenMemAccess (side splitMem) p' ty
        ifThenElseE (splitMem.split `lessEqE` p') <$> f (.top) <*> f (.bottom)
    NotSplitMem _ -> do
        let v = memAccE ty p mem
        noteModelExpr p
        noteModelExpr v
        return v

addImpliesStackEq :: MonadRepGraphFlatten m => Expr -> Expr -> Expr -> m Expr
addImpliesStackEq sp stack1 stack2 = fmap (varE memT) $ withMapSlot #cachedImpliesStackEqs (sp, stack1, stack2) $ do
    addr <- varFromNameTyE <$> addVar "stack-eq-witness" word32T
    assertFact $ (addr `bitwiseAndE` machineWordE 0x00000003) `eqE` machineWordE 0x00000000
    assertFact $ sp `lessEqE` addr
    let f = memAccE word32T addr
    (.name) <$> addDef "stack-eq" (f stack1 `eqE` f stack2)

getStackEqImplies :: MonadRepGraphFlatten m => Expr -> Expr -> m Expr
getStackEqImplies stack1 stack2 = do
    let SplitMem stack1SplitMem = tryDestructSplitMem stack1
    let (rhs, cond) = case tryDestructSplitMem stack2 of
            SplitMem (DestructSplitMem { split, top }) -> (top, split `lessEqE` stack1SplitMem.split)
            NotSplitMem s -> (s, trueE)
    noteModelExpr $ stack1SplitMem.top `eqE` rhs
    return $ cond `impliesE` (stack1SplitMem.top `eqE` rhs)

addPValids :: MonadRepGraphFlatten m => Expr -> PValidKind -> PValidType -> Expr -> m Expr
addPValids = go
  where
    go htd pvKind pvTy ptr = case htd.value of
        ExprValueOp OpIfThenElse [cond, l, r] ->
            ifThenElseE cond
                <$> go l pvKind pvTy ptr
                <*> go r pvKind pvTy ptr
        ExprValueVar name -> do
            new <- liftFlatten $ (#pvalids % at name) %%= \slot ->
                (isNothing slot, Just (fromMaybe M.empty slot))
            when new $ do
                rodataPtrs <- askRODataPtrs
                for_ rodataPtrs $ \(Expr (ExprTypePtr roTy) (ExprValueNum roAddr)) -> do
                    assertFact =<<
                        goAssumingAlreadyExists name PValidKindPGlobalValid (PValidTypeType roTy) (machineWordE roAddr)
            goAssumingAlreadyExists name pvKind pvTy ptr
    goAssumingAlreadyExists htd pvKind pvTy ptrExpr = do
        ptr <- cacheExpr "ptr" ptrExpr
        let key = PValidKey { pvKind, pvTy, ptr }
        opt <- liftFlatten $ use $ #pvalids % expectingAt htd % at key
        whenNothing opt $ do
            var <- varFromNameTyE <$> addVar "pvalid" boolT
            let info = mkPvInfo key var
            do
                fact <- alignValidIneq info.pvTy info.p
                withoutEnv $ assertFact $ info.pv `impliesE` fact
            others <- liftFlatten $ #pvalids % expectingAt htd <<%= M.insert key var
            for_ (sortOthersCompat others) $ \(otherKey, otherVar) -> do
                let otherInfo = mkPvInfo otherKey otherVar
                let pvKinds :: [PValidKind] = [otherInfo.pvKind, info.pvKind]
                unless (PValidKindPWeakValid `elem` pvKinds && PValidKindPGlobalValid `notElem` pvKinds) $ do
                    let applyAssertion f = do
                            fact <- f info otherInfo
                            withoutEnv $ assertFact fact
                    applyAssertion pvalidAssertion1
                    applyAssertion pvalidAssertion2
            return var
    mkPvInfo (PValidKey { pvKind, pvTy, ptr }) var = PValidInfo
        { pvKind
        , pvTy
        , p = varFromNameTyE ptr
        , pv = var
        }
    -- HACK matches graph-refine
    sortOthersCompat = sortOn snd . M.toList

--

tryDestructSplitMem :: Expr -> TryDestructSplitMem
tryDestructSplitMem = \case
    Expr ExprTypeMem (ExprValueOp (OpExt OpExtSplitMem) [split, top, bottom]) ->
        SplitMem $ DestructSplitMem { split, top, bottom }
    expr -> NotSplitMem expr

data TryDestructSplitMem
  = SplitMem DestructSplitMem
  | NotSplitMem Expr
  deriving (Eq, Generic, NFData, Ord, Show)

data DestructSplitMem
  = DestructSplitMem
      { split :: Expr
      , top :: Expr
      , bottom :: Expr
      }
  deriving (Eq, Generic, NFData, Ord, Show)
