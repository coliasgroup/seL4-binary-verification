{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.Core.RepGraph.New.Flatten2
    ( FlatExpr
    , FlatExprCommand
    , FlatExprContext (..)
    , FlattenEnv
    , FlattenState
    , MonadRepGraphFlatten (..)
    , convertFlatExpr
    , initFlattenEnv
    , initFlattenState
    , sendFlatCommand
    ) where

import BV.Core.RepGraph.New.Solver

import BV.Core.GenerateFreshName (generateFreshName)
import BV.Core.Logic
import BV.Core.Structs (MonadStructs)
import BV.Core.Types hiding (SplitMem (..))
import BV.Core.Types.Extras
import BV.Core.Utils
import BV.Utils

import BV.Core.RepGraph.New.ExprCommand
import Control.DeepSeq (NFData)
import Control.Monad (unless, when, (>=>))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (Reader, ReaderT, asks, runReaderT)
import Control.Monad.RWS (MonadState (get), RWST)
import Control.Monad.State (StateT, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%%=), (%=), (<<%=))

-- import Debug.Trace (traceShowM)

--

newtype FlatExprContext
  = FlatExprContext Void

type FlatExpr = Expr FlatExprContext

type FlatExprCommand = ExprCommand FlatExprContext

--

class (MonadStructs m, MonadRepGraphSolver m) => MonadRepGraphFlatten m where
    liftFlatten :: StateT FlattenState (Reader FlattenEnv) a -> m a

instance (Monoid w, MonadRepGraphFlatten m) => MonadRepGraphFlatten (RWST r w s m) where
    liftFlatten = lift . liftFlatten

instance MonadRepGraphFlatten m => MonadRepGraphFlatten (ReaderT r m) where
    liftFlatten = lift . liftFlatten

instance MonadRepGraphFlatten m => MonadRepGraphFlatten (StateT s m) where
    liftFlatten = lift . liftFlatten

instance MonadRepGraphFlatten m => MonadRepGraphFlatten (MaybeT m) where
    liftFlatten = lift . liftFlatten

instance MonadRepGraphFlatten m => MonadRepGraphFlatten (ExceptT e m) where
    liftFlatten = lift . liftFlatten

data FlattenEnv
  = FlattenEnv
      { rodataPtrs :: [SolverExpr]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data FlattenState
  = FlattenState
      { names :: Set Ident
      , exprMap :: Map Ident SolverExpr
      , defs :: Map Ident SolverExpr
      , exprCache :: Map SolverExpr Ident
      , impliesStackEqCache :: Map ImpliesStackEqCacheKey Ident
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
    , exprMap = M.empty
    , defs = M.empty
    , exprCache = M.empty
    , impliesStackEqCache = M.empty
    , pvalids = M.empty
    , modelVars = S.empty
    , modelExprs = M.empty
    }

--

send :: MonadRepGraphFlatten m => SolverExprCommand -> m ()
send = sendSolverCommand

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
    let name = Ident (generateFreshName isTaken nameHint)
    modify $ S.insert name
    return name

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

lookupDef :: MonadRepGraphFlatten m => Ident -> m (Maybe SolverExpr)
lookupDef name = liftFlatten $ use $ #defs % at name

addDefSplitMem :: MonadRepGraphFlatten m => NameHint -> SolverExpr -> m SolverExpr
addDefSplitMem nameHint expr =
    either varFromNameTyE reconstructSplitMem
        <$> addDefWithInlineInner ExprCommandInlineHintSometimes nameHint expr

addDef :: MonadRepGraphFlatten m => NameHint -> SolverExpr -> m NameTy
addDef = addDefWithInline ExprCommandInlineHintNever

addDefWithInline :: MonadRepGraphFlatten m => ExprCommandInlineHint -> NameHint -> SolverExpr -> m NameTy
addDefWithInline inline nameHint expr = viewExpecting #_Left <$> addDefWithInlineInner inline nameHint expr

addDefWithInlineSplitMem :: MonadRepGraphFlatten m => ExprCommandInlineHint -> NameHint -> SolverExpr -> m SolverExpr
addDefWithInlineSplitMem inline nameHint expr = addDefWithInlineInner inline nameHint expr <&> \case
    Left var' -> varFromNameTyE var'
    Right splitMem -> reconstructSplitMem splitMem

addDefWithInlineInner :: MonadRepGraphFlatten m => ExprCommandInlineHint -> NameHint -> SolverExpr -> m (Either NameTy DestructSplitMem)
addDefWithInlineInner inline nameHint expr = case tryDestructSplitMem (id expr) of
    NotSplitMem _ -> Left <$> do
        name <- takeFreshName nameHint
        let var = NameTy name expr.ty
        unless (isTypeOmitted expr.ty) $ do
            send $ ExprCommandDefine inline var expr
            liftFlatten $ #defs %= M.insert name expr
            when (isTypeRepresentable expr.ty) $ do
                liftFlatten $ #modelVars %= S.insert name
        return var
    SplitMem splitMem -> Right <$> do
        let add suffix = fmap varFromNameTyE <$> addDef (nameHint ++ "_" ++ suffix)
        split <- add "split" splitMem.split
        top <- add "top" splitMem.top
        bottom <- add "bot" splitMem.bottom
        return $ DestructSplitMem
            { split
            , top
            , bottom
            }

addVar :: MonadRepGraphFlatten m => NameHint -> ExprType -> m NameTy
addVar nameHint ty = do
    name <- takeFreshName nameHint
    let var = NameTy name ty
    unless (isTypeOmitted ty) $ do
        send $ ExprCommandDeclare var
        when (isTypeRepresentable ty) $ do
            liftFlatten $ #modelVars %= S.insert name
    return var

cacheExprWithInline :: MonadRepGraphFlatten m => ExprCommandInlineHint -> NameHint -> SolverExpr -> m NameTy
cacheExprWithInline inline nameHint expr = case expr.value of
    ExprValueVar name -> return $ NameTy name expr.ty
    _ -> do
        name <- withMapSlot #exprCache expr $ (.name) <$> addDefWithInline inline nameHint expr
        return $ NameTy name expr.ty

cacheExpr :: MonadRepGraphFlatten m => NameHint -> SolverExpr -> m NameTy
cacheExpr = cacheExprWithInline ExprCommandInlineHintNever

cacheExprInline :: MonadRepGraphFlatten m => NameHint -> SolverExpr -> m NameTy
cacheExprInline = cacheExprWithInline ExprCommandInlineHintSometimes

cachePtr :: MonadRepGraphFlatten m => SolverExpr -> m NameTy
cachePtr = cacheExpr "ptr"

noteModelExpr :: MonadRepGraphFlatten m => SolverExpr -> m ()
noteModelExpr expr = void $ withMapSlot #modelExprs expr $ (.name) <$> addDef "query" expr

maybeNoteModelExpr :: MonadRepGraphFlatten m => SolverExpr -> [SolverExpr] -> m ()
maybeNoteModelExpr expr subexprs =
    when (isTypeRepresentable expr.ty && not (all (isTypeRepresentable . (.ty)) subexprs)) $ do
        noteModelExpr expr

assertFact :: MonadRepGraphFlatten m => SolverExpr -> m ()
assertFact = send . ExprCommandAssert

--

sendFlatCommand :: MonadRepGraphFlatten m => FlatExprCommand -> m ()
sendFlatCommand cmd = do
    -- traceShowM cmd
    sendFlatCommand' cmd

sendFlatCommand' :: MonadRepGraphFlatten m => FlatExprCommand -> m ()
sendFlatCommand' = \case
    ExprCommandDeclare var -> do
        val' <- varFromNameTyE <$> addVar var.name.unwrap var.ty
        liftFlatten $ #exprMap %= M.insertWith undefined var.name val'
    ExprCommandDefine inlineHint var val -> do
        val' <- convertFlatExpr val >>= addDefWithInlineSplitMem inlineHint var.name.unwrap
        liftFlatten $ #exprMap %= M.insertWith undefined var.name val'
    ExprCommandAssert expr -> do
        assertFact =<< convertFlatExpr expr

convertFlatExpr :: MonadRepGraphFlatten m => FlatExpr -> m SolverExpr
convertFlatExpr = traverseOf (exprArgs % traversed) convertFlatExpr >=> \expr -> case expr.value of
    ExprValueVar name -> do
        -- let err = error $ "env miss: " ++ show name
        liftFlatten $ use $ #exprMap % expectingAt name
    ExprValueOp op args -> flattenOpExpr expr.ty op args
    _ -> return expr

--

flattenOpExprs :: MonadRepGraphFlatten m => SolverExpr -> m SolverExpr
flattenOpExprs expr = case expr.value of
    ExprValueOp op args -> traverse flattenOpExprs args >>= flattenOpExpr expr.ty op
    _ -> return expr

flattenTopLevelExpr :: MonadRepGraphFlatten m => SolverExpr -> m SolverExpr
flattenTopLevelExpr expr = case expr.value of
    ExprValueOp op args -> flattenOpExpr expr.ty op args
    _ -> return expr

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
    -- checkSplitMemInvariantM expr
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

-- TODO pass type through withMapSlot
addImpliesStackEq :: MonadRepGraphFlatten m => ImpliesStackEqCacheKey -> m SolverExpr
addImpliesStackEq key = fmap (varE boolT) $ withMapSlot #impliesStackEqCache key $ do
    addr <- varFromNameTyE <$> addVar "stack-eq-witness" word32T
    assertFact $ (addr `bitwiseAndE` machineWordE 0x00000003) `eqE` machineWordE 0x00000000
    assertFact $ key.sp `lessEqE` addr
    let f = flattenTopLevelExpr . memAccE word32T addr
    solverExpr <- eqE <$> f key.stack1 <*> f key.stack2 >>= flattenTopLevelExpr
    (.name) <$> addDef "stack-eq" solverExpr

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

isSplitMem :: SolverExpr -> Bool
isSplitMem = \case
    Expr _ (ExprValueOp (OpExt OpExtSplitMem) _) -> True
    _ -> False

tryDestructSplitMem :: SolverExpr -> TryDestructSplitMem
tryDestructSplitMem = \case
    Expr ExprTypeMem (ExprValueOp (OpExt OpExtSplitMem) [split, top, bottom]) ->
        SplitMem $ DestructSplitMem { split, top, bottom }
    expr -> NotSplitMem expr

reconstructSplitMem :: DestructSplitMem -> SolverExpr
reconstructSplitMem splitMem = splitMemE splitMem.split splitMem.top splitMem.bottom

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

--

checkSplitMemInvariantId :: SolverExpr -> SolverExpr
checkSplitMemInvariantId expr =
    if checkSplitMemInvariant expr
    then expr
    else error $ "split mem invariant not satisfied: " ++ show expr

checkSplitMemInvariantM :: Monad m => SolverExpr -> m ()
checkSplitMemInvariantM expr = do
    if checkSplitMemInvariant expr
    then pure ()
    else error $ "split mem invariant not satisfied: " ++ show expr

checkSplitMemInvariant :: SolverExpr -> Bool
checkSplitMemInvariant = go True
  where
    go isAllowed expr = case expr.value of
        ExprValueOp op args ->
            (op /= OpExt OpExtSplitMem || isAllowed)
                && all (go (expr.ty == ExprTypeRelWrapper)) args
        _ -> True
