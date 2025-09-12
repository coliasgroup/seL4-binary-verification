{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.Core.Experimental.RepGraph.Flatten
    ( ExprEnv
    , FlattenEnv
    , FlattenState
    , MonadRepGraphFlatten (..)
    , MonadRepGraphFlattenSend (..)
    , NameHint
    , addDef
    , addVar
    , assertFact
    , cacheExpr
    , cacheExprInline
    , exprEnvVars
    , flattenAndAddDef
    , flattenAndAssertFact
    , flattenExpr
    , flattenOpExpr
    , getDef
    , getModelExprs
    , getModelVars
    , initFlattenEnv
    , initFlattenState
    , lookupDef
    , withEnv
    , withoutEnv
    ) where

import BV.Core.Experimental.RepGraph.Types

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
import Control.Monad.Reader (Reader, ReaderT, asks, runReaderT)
import Control.Monad.RWS (MonadState (get), MonadWriter (..), RWST)
import Control.Monad.State (StateT, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Writer (WriterT)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%%=), (%=), (<<%=))

--

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
      { rodataPtrs :: [SolverExpr]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data FlattenState
  = FlattenState
      { names :: Set Ident
      , defs :: Map Ident SolverExpr
      , exprCache :: Map SolverExpr Ident
      , impliesStackEqCache :: Map ImpliesStackEqCacheKey Ident
      , tokens :: Map Ident Ident
      , pvalids :: Map Ident (Map PValidKey SolverExpr)
      , modelVars :: Set Ident
      , modelExprs :: Map SolverExpr Ident
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data ImpliesStackEqCacheKey
  = ImpliesStackEqCacheKey
      { sp :: SolverExpr
      , stack1 :: SolverExpr
      , stack2 :: SolverExpr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PValidKey
  = PValidKey
      { pvKind :: PValidKind
      , pvTy :: PValidType SolverExprContext
      , ptr :: NameTy
      }
  deriving (Eq, Generic, NFData, Ord, Show)

initFlattenEnv :: [SolverExpr] -> FlattenEnv
initFlattenEnv rodataPtrs = FlattenEnv
    { rodataPtrs
    }

initFlattenState :: FlattenState
initFlattenState = FlattenState
    { names = S.empty
    , defs = M.empty
    , exprCache = M.empty
    , impliesStackEqCache = M.empty
    , tokens = M.empty
    , pvalids = M.empty
    , modelVars = S.empty
    , modelExprs = M.empty
    }

--

send :: MonadRepGraphFlatten m => Command -> m ()
send = sendCommand

--

withMapSlot :: (MonadRepGraphFlatten m, Ord k) => Lens' FlattenState (M.Map k v) -> k -> m v -> m v
withMapSlot l k m = do
    opt <- liftFlatten (use (l % at k))
    whenNothing opt $ do
        v <- m
        liftFlatten $ l %= M.insert k v
        return v

--

askRODataPtrs :: MonadRepGraphFlatten m => m [SolverExpr]
askRODataPtrs = liftFlatten $ gview #rodataPtrs

--

getModelVars :: MonadRepGraphFlatten m => m (Set Ident)
getModelVars = liftFlatten $ use #modelVars

getModelExprs :: MonadRepGraphFlatten m => m (Map SolverExpr Ident)
getModelExprs = liftFlatten $ use #modelExprs

--

type NameHint = String

takeFreshName :: MonadRepGraphFlatten m => NameHint -> m Ident
takeFreshName nameHint = liftFlatten $ zoom #names $ do
    names <- get
    let isTaken = (`S.member` names) . Ident
    let name = Ident (generateFreshName isTaken sanitized)
    modify $ S.insert name
    return name
  where
    sanitized =
        [ if c `elem` ("'#\"" :: String) then '_' else c
        | c <- nameHint
        ]

--

isTypeRepresentable :: ExprType -> Bool
isTypeRepresentable = \case
    ExprTypeWord _ -> True
    ExprTypeBool -> True
    _ -> False

getDef :: MonadRepGraphFlatten m => Ident -> m SolverExpr
getDef name = liftFlatten $ use $ #defs % expectingAt name

lookupDef :: MonadRepGraphFlatten m => Ident -> m (Maybe SolverExpr)
lookupDef name = liftFlatten $ use $ #defs % at name

addDefWithInline :: MonadRepGraphFlatten m => Maybe InlineHint -> NameHint -> SolverExpr -> m NameTy
addDefWithInline inline nameHint expr = do
    name <- takeFreshName nameHint
    let var = NameTy name expr.ty
    send $ CommandDefine inline var expr
    liftFlatten $ #defs %= M.insert name expr
    when (isTypeRepresentable expr.ty) $ do
        liftFlatten $ #modelVars %= S.insert name
    return var

addDef :: MonadRepGraphFlatten m => NameHint -> SolverExpr -> m NameTy
addDef = addDefWithInline (Just InlineHintDontInline)

addVar :: MonadRepGraphFlatten m => NameHint -> ExprType -> m NameTy
addVar nameHint ty = do
    name <- takeFreshName nameHint
    let var = NameTy name ty
    send $ CommandDeclare var
    when (isTypeRepresentable ty) $ do
        liftFlatten $ #modelVars %= S.insert name
    return var

cacheExprWithInline :: MonadRepGraphFlatten m => Maybe InlineHint -> NameHint -> SolverExpr -> m NameTy
cacheExprWithInline inline nameHint expr = case expr.value of
    ExprValueVar name -> return $ NameTy name expr.ty
    _ -> do
        name <- withMapSlot #exprCache expr $ (.name) <$> addDefWithInline inline nameHint expr
        return $ NameTy name expr.ty

cacheExpr :: MonadRepGraphFlatten m => NameHint -> SolverExpr -> m NameTy
cacheExpr = cacheExprWithInline (Just InlineHintDontInline)

cacheExprInline :: MonadRepGraphFlatten m => NameHint -> SolverExpr -> m NameTy
cacheExprInline = cacheExprWithInline (Just InlineHintInline)

cachePtr :: MonadRepGraphFlatten m => SolverExpr -> m NameTy
cachePtr = cacheExpr "ptr"

noteModelExpr :: MonadRepGraphFlatten m => SolverExpr -> m ()
noteModelExpr expr = void $ withMapSlot #modelExprs expr $ (.name) <$> addDef "query" expr

maybeNoteModelExpr :: MonadRepGraphFlatten m => SolverExpr -> [SolverExpr] -> m ()
maybeNoteModelExpr expr subexprs =
    when (isTypeRepresentable expr.ty && not (all (isTypeRepresentable . (.ty)) subexprs)) $ do
        noteModelExpr expr

getToken :: MonadRepGraphFlatten m => Ident -> m SolverExpr
getToken ident = fmap (varE compiledTokenType) $ withMapSlot #tokens ident $ do
    n <- liftFlatten $ use $ #tokens % to M.size
    (.name) <$> addDef
        ("token_" ++ ident.unwrap)
        (numE compiledTokenType (toInteger n))

compiledTokenType :: ExprType
compiledTokenType = ExprTypeWord 64

assertFact :: MonadRepGraphFlatten m => SolverExpr -> m ()
assertFact = send . CommandAssert

--

type ExprEnv = Map Ident SolverExpr

withEnv :: ExprEnv -> ReaderT ExprEnv m a -> m a
withEnv = flip runReaderT

withoutEnv :: ReaderT ExprEnv m a -> m a
withoutEnv = flip runReaderT mempty

exprEnvVars :: ExprEnv -> Set NameTy
exprEnvVars = S.fromList . M.elems . M.mapWithKey (\k v -> NameTy k v.ty)

--

flattenAndAddDef :: MonadRepGraphFlatten m => NameHint -> GraphExpr -> ReaderT ExprEnv m NameTy
flattenAndAddDef nameHint = flattenExpr >=> addDef nameHint

flattenAndAssertFact :: MonadRepGraphFlatten m => GraphExpr -> ReaderT ExprEnv m ()
flattenAndAssertFact = flattenExpr >=> assertFact

--

flattenExpr :: MonadRepGraphFlatten m => GraphExpr -> ReaderT ExprEnv m SolverExpr
flattenExpr expr = case matching exprOpArgs expr of
    Left expr' -> case expr'.value of
        ExprValueVar name -> do
            let err = error $ "env miss: " ++ show name
            asks $ M.findWithDefault err name
        ExprValueToken tok -> do
            getToken tok
        _ -> return expr'
    Right (op, args) -> traverse flattenExpr args >>= flattenOpExpr expr.ty op

flattenOpExpr :: MonadRepGraphFlatten m => ExprType -> Op -> [SolverExpr] -> m SolverExpr
flattenOpExpr exprTy op args = do
    expr <- case (op, args) of
        (OpIfThenElse, [cond, x, y]) -> do
            return $ flattenIfThenElse cond x y
        (OpMemUpdate, [m, p, v]) -> do
            flattenMemUpdate m p v v.ty
        (OpMemAcc, [m, p]) -> do
            flattenMemAccess m p exprTy
        (OpExt OpExtStackEqualsImplies, [sp1, stack1, sp2, stack2]) -> do
            if sp1 == sp2 && stack1 == stack2
            then return trueE
            else do
                eq <- getStackEqImplies stack2 stack1
                return $ (sp1 `eqE` sp2) `andE` eq
        (OpExt OpExtImpliesStackEquals, [sp1, stack1, sp2, stack2]) -> do
            eq <- addImpliesStackEq $ ImpliesStackEqCacheKey
                { sp = sp1
                , stack1
                , stack2
                }
            return $ (sp1 `eqE` sp2) `andE` eq
        (OpPArrayValid, [htd, tyExpr, ptrExpr, len]) -> do
            let ExprValueType tyVal = tyExpr.value
            ptr <- cachePtr ptrExpr
            addPValids htd $ PValidKey
                { pvKind = pvalidKindFromOp op
                , pvTy = PValidTypeArray { ty = tyVal, len }
                , ptr
                }
        (_, [htd, tyExpr, ptrExpr]) | op `elem` [OpPValid, OpPGlobalValid, OpPWeakValid] -> do
            let ExprValueType tyVal = tyExpr.value
            ptr <- cachePtr ptrExpr
            addPValids htd $ PValidKey
                { pvKind = pvalidKindFromOp op
                , pvTy = PValidTypeType tyVal
                , ptr
                }
        _ -> do
            return $ Expr exprTy (ExprValueOp op args)
    ensureM $ expr.ty == exprTy
    maybeNoteModelExpr expr args
    return expr

flattenIfThenElse :: SolverExpr -> SolverExpr -> SolverExpr -> SolverExpr
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

flattenMemUpdate :: MonadRepGraphFlatten m => SolverExpr -> SolverExpr -> SolverExpr -> ExprType -> m SolverExpr
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

flattenMemAccess :: MonadRepGraphFlatten m => SolverExpr -> SolverExpr -> ExprType -> m SolverExpr
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

addImpliesStackEq :: MonadRepGraphFlatten m => ImpliesStackEqCacheKey -> m SolverExpr
addImpliesStackEq key = fmap (varE memT) $ withMapSlot #impliesStackEqCache key $ do
    addr <- varFromNameTyE <$> addVar "stack-eq-witness" word32T
    assertFact $ (addr `bitwiseAndE` machineWordE 0x00000003) `eqE` machineWordE 0x00000000
    assertFact $ key.sp `lessEqE` addr
    let f = memAccE word32T addr
    (.name) <$> addDef "stack-eq" (f key.stack1 `eqE` f key.stack2)

getStackEqImplies :: MonadRepGraphFlatten m => SolverExpr -> SolverExpr -> m SolverExpr
getStackEqImplies stack1 stack2 = do
    let SplitMem stack1SplitMem = tryDestructSplitMem stack1
    let (rhs, cond) = case tryDestructSplitMem stack2 of
            SplitMem (DestructSplitMem { split, top }) -> (top, split `lessEqE` stack1SplitMem.split)
            NotSplitMem s -> (s, trueE)
    noteModelExpr $ stack1SplitMem.top `eqE` rhs
    return $ cond `impliesE` (stack1SplitMem.top `eqE` rhs)

addPValids :: MonadRepGraphFlatten m => SolverExpr -> PValidKey -> m SolverExpr
addPValids = go
  where
    go htd key = case htd.value of
        ExprValueOp OpIfThenElse [cond, l, r] ->
            ifThenElseE cond
                <$> go l key
                <*> go r key
        ExprValueVar name -> do
            new <- liftFlatten $ (#pvalids % at name) %%= \slot ->
                (isNothing slot, Just (fromMaybe M.empty slot))
            when new $ do
                rodataPtrs <- askRODataPtrs
                for_ rodataPtrs $ \(Expr (ExprTypePtr roTy) (ExprValueNum roAddr)) -> do
                    ptr <- cachePtr $ machineWordE roAddr
                    assertFact =<<
                        goAssumingAlreadyExists name (PValidKey
                            { pvKind = PValidKindPGlobalValid
                            , pvTy = PValidTypeType roTy
                            , ptr
                            })
            goAssumingAlreadyExists name key
    goAssumingAlreadyExists htd key = do
        opt <- liftFlatten $ use $ #pvalids % expectingAt htd % at key
        whenNothing opt $ do
            var <- varFromNameTyE <$> addVar "pvalid" boolT
            let info = mkPvInfo key var
            do
                fact <- alignValidIneq info.pvTy info.p
                assertFact $ info.pv `impliesE` fact
            others <- liftFlatten $ #pvalids % expectingAt htd <<%= M.insert key var
            for_ (M.toList others) $ \(otherKey, otherVar) -> do
                let otherInfo = mkPvInfo otherKey otherVar
                let pvKinds :: [PValidKind] = [otherInfo.pvKind, info.pvKind]
                unless (PValidKindPWeakValid `elem` pvKinds && PValidKindPGlobalValid `notElem` pvKinds) $ do
                    let applyAssertion f = do
                            fact <- f info otherInfo
                            assertFact fact
                    applyAssertion pvalidAssertion1
                    applyAssertion pvalidAssertion2
            return var
    mkPvInfo (PValidKey { pvKind, pvTy, ptr }) var = PValidInfo
        { pvKind
        , pvTy
        , p = varFromNameTyE ptr
        , pv = var
        }

--

tryDestructSplitMem :: SolverExpr -> TryDestructSplitMem
tryDestructSplitMem = \case
    Expr ExprTypeMem (ExprValueOp (OpExt OpExtSplitMem) [split, top, bottom]) ->
        SplitMem $ DestructSplitMem { split, top, bottom }
    expr -> NotSplitMem expr

data TryDestructSplitMem
  = SplitMem DestructSplitMem
  | NotSplitMem SolverExpr
  deriving (Eq, Generic, NFData, Ord, Show)

data DestructSplitMem
  = DestructSplitMem
      { split :: SolverExpr
      , top :: SolverExpr
      , bottom :: SolverExpr
      }
  deriving (Eq, Generic, NFData, Ord, Show)
