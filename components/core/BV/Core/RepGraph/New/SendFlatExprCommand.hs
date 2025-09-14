{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.Core.RepGraph.New.SendFlatExprCommand
    ( FlatExpr
    , FlatExprCommand
    , FlatExprContext (..)
    , RepGraphSendFlatExprCommandT
    , convertFlatExpr
    , getModelExprs
    , getModelVars
    , runRepGraphSendFlatExprCommandT
    , sendFlatExprCommand
    ) where

import BV.Core.RepGraph.New.ExprCommand
import BV.Core.RepGraph.New.SendSolverExprCommand

import BV.Core.GenerateFreshName (generateFreshName)
import BV.Core.Logic
import BV.Core.Structs (MonadStructs)
import BV.Core.Types hiding (SplitMem (..))
import BV.Core.Types.Extras
import BV.Core.Utils
import BV.Utils

import Control.Monad (unless, when, (>=>))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (Reader, ReaderT (runReaderT), mapReaderT)
import Control.Monad.RWS (MonadState (get))
import Control.Monad.State (StateT, evalStateT, mapStateT, modify)
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

-- import Debug.Trace (traceShowM)

type C m = (MonadStructs m, MonadRepGraphSendSExpr m)

type T = RepGraphSendFlatExprCommandT

type InnerT = RepGraphSendSolverExprCommandT

--

newtype FlatExprContext
  = FlatExprContext Void

type FlatExpr = Expr FlatExprContext

type FlatExprCommand = ExprCommand FlatExprContext

--

newtype RepGraphSendFlatExprCommandT m a
  = RepGraphSendFlatExprCommandT { run :: StateT TState (ReaderT TEnv (InnerT m)) a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance MonadTrans RepGraphSendFlatExprCommandT where
    lift = RepGraphSendFlatExprCommandT . lift . lift . lift

liftStructs :: MonadStructs m => m a -> T m a
liftStructs = lift

liftPure :: Monad m => StateT TState (Reader TEnv) a -> T m a
liftPure = RepGraphSendFlatExprCommandT . mapStateT (mapReaderT (return . runIdentity))

liftInner :: Monad m => InnerT m a -> T m a
liftInner = RepGraphSendFlatExprCommandT . lift . lift

send :: C m => SolverExprCommand -> T m ()
send = liftInner . sendSolverExprCommand

runRepGraphSendFlatExprCommandT :: Monad m => ROData -> T m a -> m a
runRepGraphSendFlatExprCommandT rodata =
      runRepGraphSendSolverExprCommandT rodata
    . flip runReaderT (initEnv (rodataPtrsFromROData rodata))
    . flip evalStateT initState
    . (.run)

rodataPtrsFromROData :: ROData -> [SolverExpr]
rodataPtrsFromROData rodata =
    [ pointerE (structT structName) (machineWordE range.addr)
    | (structName, range) <- rodataStructNamesOf rodata
    ]

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
withMapSlot l k m = do
    opt <- liftPure (use (l % at k))
    whenNothing opt $ do
        v <- m
        liftPure $ l %= M.insert k v
        return v

--

askRODataPtrs :: C m => T m [SolverExpr]
askRODataPtrs = liftPure $ gview #rodataPtrs

--

getModelVars :: C m => T m (Set Ident)
getModelVars = liftPure $ use #modelVars

getModelExprs :: C m => T m (Map SolverExpr Ident)
getModelExprs = liftPure $ use #modelExprs

--

type NameHint = String

takeFreshName :: C m => NameHint -> T m Ident
takeFreshName nameHint = liftPure $ zoom #names $ do
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

addDef :: C m => NameHint -> SolverExpr -> T m NameTy
addDef = addDefWithInline ExprCommandInlineHintNever

addDefWithInline :: C m => ExprCommandInlineHint -> NameHint -> SolverExpr -> T m NameTy
addDefWithInline inline nameHint expr = viewExpecting #_Left <$> addDefWithInlineInner inline nameHint expr

addDefWithInlineSplitMem :: C m => ExprCommandInlineHint -> NameHint -> SolverExpr -> T m SolverExpr
addDefWithInlineSplitMem inline nameHint expr = addDefWithInlineInner inline nameHint expr <&> \case
    Left var' -> varFromNameTyE var'
    Right splitMem -> reconstructSplitMem splitMem

addDefWithInlineInner :: C m => ExprCommandInlineHint -> NameHint -> SolverExpr -> T m (Either NameTy DestructSplitMem)
addDefWithInlineInner inline nameHint expr = case tryDestructSplitMem (id expr) of
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

addVar :: C m => NameHint -> ExprType -> T m NameTy
addVar nameHint ty = do
    name <- takeFreshName nameHint
    let var = NameTy name ty
    unless (isTypeOmitted ty) $ do
        send $ ExprCommandDeclare var
        when (isTypeRepresentable ty) $ do
            liftPure $ #modelVars %= S.insert name
    return var

cacheExprWithInline :: C m => ExprCommandInlineHint -> NameHint -> SolverExpr -> T m NameTy
cacheExprWithInline inline nameHint expr = case expr.value of
    ExprValueVar name -> return $ NameTy name expr.ty
    _ -> do
        name <- withMapSlot #exprCache expr $ (.name) <$> addDefWithInline inline nameHint expr
        return $ NameTy name expr.ty

cacheExpr :: C m => NameHint -> SolverExpr -> T m NameTy
cacheExpr = cacheExprWithInline ExprCommandInlineHintNever

cacheExprInline :: C m => NameHint -> SolverExpr -> T m NameTy
cacheExprInline = cacheExprWithInline ExprCommandInlineHintSometimes

cachePtr :: C m => SolverExpr -> T m NameTy
cachePtr = cacheExpr "ptr"

noteModelExpr :: C m => SolverExpr -> T m ()
noteModelExpr expr = void $ withMapSlot #modelExprs expr $ (.name) <$> addDef "query" expr

maybeNoteModelExpr :: C m => SolverExpr -> [SolverExpr] -> T m ()
maybeNoteModelExpr expr subexprs =
    when (isTypeRepresentable expr.ty && not (all (isTypeRepresentable . (.ty)) subexprs)) $ do
        noteModelExpr expr

assertFact :: C m => SolverExpr -> T m ()
assertFact = send . ExprCommandAssert

--

sendFlatExprCommand :: C m => FlatExprCommand -> T m ()
sendFlatExprCommand cmd = do
    -- traceShowM cmd
    sendFlatExprCommand' cmd

sendFlatExprCommand' :: C m => FlatExprCommand -> T m ()
sendFlatExprCommand' = \case
    ExprCommandDeclare var -> do
        val' <- varFromNameTyE <$> addVar var.name.unwrap var.ty
        liftPure $ #exprMap %= M.insertWith undefined var.name val'
    ExprCommandDefine inlineHint var val -> do
        val' <- convertFlatExpr val >>= addDefWithInlineSplitMem inlineHint var.name.unwrap
        liftPure $ #exprMap %= M.insertWith undefined var.name val'
    ExprCommandAssert expr -> do
        assertFact =<< convertFlatExpr expr

convertFlatExpr :: C m => FlatExpr -> T m SolverExpr
convertFlatExpr = traverseOf (exprArgs % traversed) convertFlatExpr >=> \expr -> case expr.value of
    ExprValueVar name -> do
        -- let err = error $ "env miss: " ++ show name
        liftPure $ use $ #exprMap % expectingAt name
    ExprValueOp op args -> flattenOpExpr expr.ty op args
    _ -> return expr

--

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

-- TODO pass type through withMapSlot
addImpliesStackEq :: C m => ImpliesStackEqCacheKey -> T m SolverExpr
addImpliesStackEq key = fmap (varE boolT) $ withMapSlot #impliesStackEqCache key $ do
    addr <- varFromNameTyE <$> addVar "stack-eq-witness" word32T
    assertFact $ (addr `bitwiseAndE` machineWordE 0x00000003) `eqE` machineWordE 0x00000000
    assertFact $ key.sp `lessEqE` addr
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
                    assertFact =<<
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
                assertFact $ info.pv `impliesE` fact
            others <- liftPure $ #pvalids % expectingAt htd <<%= M.insert key var
            for_ (M.toList others) $ \(otherKey, otherVar) -> do
                let otherInfo = mkPvInfo otherKey otherVar
                let pvKinds :: [PValidKind] = [otherInfo.pvKind, info.pvKind]
                unless (PValidKindPWeakValid `elem` pvKinds && PValidKindPGlobalValid `notElem` pvKinds) $ do
                    let applyAssertion f = do
                            fact <- liftStructs $ f info otherInfo
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
