{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.Core.RepGraph.New.SendFlatExprCommand
    ( FlatExpr
    , FlatExprCommand
    , FlatExprContext (..)
    , RepGraphSendFlatExprCommandT
    , convertFlatExpr
    , runRepGraphSendFlatExprCommandTStep
    , sendFlatExprCommand
    ) where

import BV.Core.RepGraph.New.Common
import BV.Core.RepGraph.New.SendSolverExprCommand

import BV.Core.GenerateFreshName (takeFreshNameWith)
import BV.Core.Logic
import BV.Core.Structs (StructsT, runStructsT)
import BV.Core.Types hiding (SplitMem (..))
import BV.Core.Types.Extras
import BV.Core.Utils (whenNothing, withMapSlotWith)
import BV.Utils (ensureM, expectingAt, viewExpecting)

import Control.Monad (unless, when, (>=>))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (Reader, ReaderT, mapReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT, mapStateT)
import Control.Monad.Trans (MonadTrans, lift)
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

type T = RepGraphSendFlatExprCommandT

type InnerT = RepGraphSendSolverExprCommandT

type C = MonadRepGraphSendSExpr

--

newtype FlatExprContext
  = FlatExprContext Void

type FlatExpr = Expr FlatExprContext

type FlatExprCommand = ExprCommand FlatExprContext

--

newtype RepGraphSendFlatExprCommandT m a
  = RepGraphSendFlatExprCommandT { run :: StateT TState (ReaderT TEnv (StructsT (InnerT m))) a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance Monad m => MonadInner (InnerT m) (T m) where
    liftInner = RepGraphSendFlatExprCommandT . lift . lift . lift

instance MonadTrans T where
    lift = liftInner . lift

liftStructs :: Monad m => StructsT (InnerT m) a -> T m a
liftStructs = RepGraphSendFlatExprCommandT . lift . lift

liftPure :: Monad m => StateT TState (Reader TEnv) a -> T m a
liftPure = RepGraphSendFlatExprCommandT . mapStateT (mapReaderT (return . runIdentity))

send :: C m => SolverExprCommand -> T m ()
send = liftInner . sendSolverExprCommand

runRepGraphSendFlatExprCommandTStep :: Monad m => (Ident -> Struct) -> [SolverExpr] -> T m a -> InnerT m a
runRepGraphSendFlatExprCommandTStep lookupStruct rodataPtrs =
      runStructsT lookupStruct
    . flip runReaderT (initEnv rodataPtrs)
    . flip evalStateT initState
    . (.run)

data TEnv
  = TEnv
      { rodataPtrs :: [SolverExpr]
      }
  deriving (Generic)

data TState
  = TState
      { names :: Set Ident
      , exprMap :: Map Ident SolverExpr
      , exprCache :: Map SolverExpr Ident
      , impliesStackEqCache :: Map ImpliesStackEqCacheKey Ident
      , pvalids :: Map Ident (Map PValidKey SolverExpr)
      , modelVars :: Set Ident
      , modelExprs :: Map SolverExpr Ident
      }
  deriving (Generic)

data ImpliesStackEqCacheKey
  = ImpliesStackEqCacheKey
      { sp :: SolverExpr
      , stack1 :: SolverExpr
      , stack2 :: SolverExpr
      }
  deriving (Eq, Generic, Ord)

data PValidKey
  = PValidKey
      { pvKind :: PValidKind
      , pvTy :: PValidType SolverExprContext
      , ptr :: NameTy
      }
  deriving (Eq, Generic, Ord)

initEnv :: [SolverExpr] -> TEnv
initEnv rodataPtrs = TEnv
    { rodataPtrs
    }

initState :: TState
initState = TState
    { names = S.empty
    , exprMap = M.empty
    , exprCache = M.empty
    , impliesStackEqCache = M.empty
    , pvalids = M.empty
    , modelVars = S.empty
    , modelExprs = M.empty
    }

--

withMapSlot :: (C m, Ord k) => Lens' TState (M.Map k v) -> k -> T m v -> T m v
withMapSlot = withMapSlotWith $ liftPure . mapStateT (return . runIdentity)

askRODataPtrs :: C m => T m [SolverExpr]
askRODataPtrs = liftPure $ gview #rodataPtrs

type NameHint = String

takeFreshName :: C m => NameHint -> T m Ident
takeFreshName nameHint = liftPure $ zoom #names $ takeFreshNameWith Ident nameHint

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

addVar :: C m => NameHint -> ExprType -> T m NameTy
addVar nameHint ty = do
    name <- takeFreshName nameHint
    let var = NameTy name ty
    unless (isTypeOmitted ty) $ do
        send $ ExprCommandDeclare var
        when (isTypeRepresentable ty) $ do
            liftPure $ #modelVars %= S.insert name
    return var

addDef :: C m => NameHint -> SolverExpr -> T m NameTy
addDef = addDefWithInlineHint ExprCommandInlineHintNever

addDefWithInlineHint :: C m => ExprCommandInlineHint -> NameHint -> SolverExpr -> T m NameTy
addDefWithInlineHint inline nameHint expr = viewExpecting #_Left <$> addDefWithInlineHintInner inline nameHint expr

addDefWithInlineHintSplitMem :: C m => ExprCommandInlineHint -> NameHint -> SolverExpr -> T m SolverExpr
addDefWithInlineHintSplitMem inline nameHint expr = addDefWithInlineHintInner inline nameHint expr <&> \case
    Left var' -> varFromNameTyE var'
    Right splitMem -> reconstructSplitMem splitMem

addDefWithInlineHintInner :: C m => ExprCommandInlineHint -> NameHint -> SolverExpr -> T m (Either NameTy DestructSplitMem)
addDefWithInlineHintInner inline nameHint expr = case tryDestructSplitMem (id expr) of
    NotSplitMem _ -> Left <$> do
        name <- takeFreshName nameHint
        let var = NameTy name expr.ty
        unless (isTypeOmitted expr.ty) $ do
            send $ ExprCommandDefine inline var expr
            when (isTypeRepresentable expr.ty) $ do
                liftPure $ #modelVars %= S.insert name
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

cacheExpr :: C m => NameHint -> SolverExpr -> T m NameTy
cacheExpr = cacheExprWithInlineHint ExprCommandInlineHintNever

cacheExprInline :: C m => NameHint -> SolverExpr -> T m NameTy
cacheExprInline = cacheExprWithInlineHint ExprCommandInlineHintSometimes

cacheExprWithInlineHint :: C m => ExprCommandInlineHint -> NameHint -> SolverExpr -> T m NameTy
cacheExprWithInlineHint inline nameHint expr = flip NameTy expr.ty <$> case expr.value of
    ExprValueVar name -> return name
    _ -> withMapSlot #exprCache expr $ (.name) <$> addDefWithInlineHint inline nameHint expr

cachePtr :: C m => SolverExpr -> T m NameTy
cachePtr = cacheExpr "ptr"

noteModelExpr :: C m => SolverExpr -> T m ()
noteModelExpr expr = void $ withMapSlot #modelExprs expr $ (.name) <$> addDef "query" expr

maybeNoteModelExpr :: C m => SolverExpr -> [SolverExpr] -> T m ()
maybeNoteModelExpr expr subexprs =
    when (isTypeRepresentable expr.ty && not (all (isTypeRepresentable . (.ty)) subexprs)) $ do
        noteModelExpr expr

assertSolverExpr :: C m => SolverExpr -> T m ()
assertSolverExpr = send . ExprCommandAssert

--

sendFlatExprCommand :: C m => FlatExprCommand -> T m ()
sendFlatExprCommand = \case
    ExprCommandDeclare var -> do
        val' <- varFromNameTyE <$> addVar var.name.unwrap var.ty
        liftPure $ #exprMap %= M.insertWith undefined var.name val'
    ExprCommandDefine inlineHint var val -> do
        val' <- convertFlatExpr val >>= addDefWithInlineHintSplitMem inlineHint var.name.unwrap
        liftPure $ #exprMap %= M.insertWith undefined var.name val'
    ExprCommandAssert expr -> do
        assertSolverExpr =<< convertFlatExpr expr

convertFlatExpr :: C m => FlatExpr -> T m SolverExpr
convertFlatExpr = traverseOf (exprArgs % traversed) convertFlatExpr >=> \expr -> case expr.value of
    ExprValueVar name -> liftPure $ use $ #exprMap % expectingAt name
    ExprValueOp op args -> flattenOpExpr expr.ty op args
    _ -> return expr

flattenOpExpr :: C m => ExprType -> Op -> [SolverExpr] -> T m SolverExpr
flattenOpExpr exprTy op args = do
    expr <- case (op, args) of
        (OpIfThenElse, [cond, x, y]) -> do
            return $ flattenIfThenElse cond x y
        (OpMemUpdate, [m, p, v]) -> do
            flattenMemUpdate m p v v.ty
        (OpMemAcc, [m, p]) -> do
            flattenMemAccess exprTy m p
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

flattenMemUpdate :: C m => SolverExpr -> SolverExpr -> SolverExpr -> ExprType -> T m SolverExpr
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

flattenMemAccess :: C m => ExprType -> SolverExpr -> SolverExpr -> T m SolverExpr
flattenMemAccess ty@(ExprTypeWord _) mem p = case tryDestructSplitMem mem of
    SplitMem splitMem -> do
        p' <- varFromNameTyE <$> cacheExprInline "memacc_pointer" p
        let f side = flattenMemAccess ty (side splitMem) p'
        ifThenElseE (splitMem.split `lessEqE` p') <$> f (.top) <*> f (.bottom)
    NotSplitMem _ -> do
        let v = memAccE ty p mem
        noteModelExpr p
        noteModelExpr v
        return v

addImpliesStackEq :: C m => ImpliesStackEqCacheKey -> T m SolverExpr
addImpliesStackEq key = fmap (varE boolT) $ withMapSlot #impliesStackEqCache key $ do
    addr <- varFromNameTyE <$> addVar "stack-eq-witness" word32T
    assertSolverExpr $ (addr `bitwiseAndE` machineWordE 0x00000003) `eqE` machineWordE 0x00000000
    assertSolverExpr $ key.sp `lessEqE` addr
    let f mem = flattenMemAccess word32T mem addr
    solverExpr <- eqE <$> f key.stack1 <*> f key.stack2
    (.name) <$> addDef "stack-eq" solverExpr

getStackEqImplies :: C m => SolverExpr -> SolverExpr -> T m SolverExpr
getStackEqImplies stack1 stack2 = do
    let SplitMem stack1SplitMem = tryDestructSplitMem stack1
    let (rhs, cond) = case tryDestructSplitMem stack2 of
            SplitMem (DestructSplitMem { split, top }) -> (top, split `lessEqE` stack1SplitMem.split)
            NotSplitMem s -> (s, trueE)
    noteModelExpr $ stack1SplitMem.top `eqE` rhs
    return $ cond `impliesE` (stack1SplitMem.top `eqE` rhs)

addPValids :: C m => SolverExpr -> PValidKey -> T m SolverExpr
addPValids = go
  where
    go htd key = case htd.value of
        ExprValueOp OpIfThenElse [cond, l, r] ->
            ifThenElseE cond
                <$> go l key
                <*> go r key
        ExprValueVar name -> do
            new <- liftPure $ (#pvalids % at name) %%= \slot ->
                (isNothing slot, Just (fromMaybe M.empty slot))
            when new $ do
                rodataPtrs <- askRODataPtrs
                for_ rodataPtrs $ \(Expr (ExprTypePtr roTy) (ExprValueNum roAddr)) -> do
                    ptr <- cachePtr $ machineWordE roAddr
                    assertSolverExpr =<<
                        goAssumingAlreadyExists name (PValidKey
                            { pvKind = PValidKindPGlobalValid
                            , pvTy = PValidTypeType roTy
                            , ptr
                            })
            goAssumingAlreadyExists name key
    goAssumingAlreadyExists htd key = do
        opt <- liftPure $ use $ #pvalids % expectingAt htd % at key
        whenNothing opt $ do
            var <- varFromNameTyE <$> addVar "pvalid" boolT
            let info = mkPvInfo key var
            do
                fact <- liftStructs $ alignValidIneq info.pvTy info.p
                assertSolverExpr $ info.pv `impliesE` fact
            others <- liftPure $ #pvalids % expectingAt htd <<%= M.insert key var
            for_ (M.toList others) $ \(otherKey, otherVar) -> do
                let otherInfo = mkPvInfo otherKey otherVar
                let pvKinds :: [PValidKind] = [otherInfo.pvKind, info.pvKind]
                unless (PValidKindPWeakValid `elem` pvKinds && PValidKindPGlobalValid `notElem` pvKinds) $ do
                    let applyAssertion f = do
                            fact <- liftStructs $ f info otherInfo
                            assertSolverExpr fact
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

reconstructSplitMem :: DestructSplitMem -> SolverExpr
reconstructSplitMem splitMem = splitMemE splitMem.split splitMem.top splitMem.bottom

data TryDestructSplitMem
  = SplitMem DestructSplitMem
  | NotSplitMem SolverExpr
  deriving (Eq, Generic, Ord, Show)

data DestructSplitMem
  = DestructSplitMem
      { split :: SolverExpr
      , top :: SolverExpr
      , bottom :: SolverExpr
      }
  deriving (Eq, Generic, Ord, Show)
