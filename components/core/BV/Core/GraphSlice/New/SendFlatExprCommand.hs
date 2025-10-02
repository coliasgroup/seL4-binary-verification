{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.GraphSlice.New.SendFlatExprCommand
    ( FlatExpr
    , FlatExprCommand
    , FlatExprContext (..)
    , GraphSliceSendFlatExprCommandT
    , convertFlatExpr
    , runGraphSliceSendFlatExprCommandTStep
    , sendAccumulatedAssertionsInner
    , sendFlatExprCommand
    ) where

import BV.Core.GraphSlice.New.Common
import BV.Core.GraphSlice.New.SendSolverExprCommand

import BV.Core.GenerateFreshName (takeFreshNameWith)
import BV.Core.Logic
import BV.Core.Structs (StructsT, mapStructsT, runStructsT)
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils (withMapSlotWith)
import BV.Utils (ensureM, expectingAt, viewExpecting)

import Control.Monad (unless, when)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (Reader, ReaderT, mapReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT, mapStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (for_, sequenceA_)
import Data.Function (on)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))

-- import Debug.Trace (traceShowM)

-- TODO explicitly weaken pvalid ops in strengthenHyp

-- TODO
cheatMemDoms :: Bool
cheatMemDoms = True

--

type T = GraphSliceSendFlatExprCommandT

type InnerT = GraphSliceSendSolverExprCommandT

type C = MonadGraphSliceSendSExpr

--

newtype FlatExprContext
  = FlatExprContext Void

type FlatExpr = Expr FlatExprContext

type FlatExprCommand = ExprCommand FlatExprContext

--

newtype GraphSliceSendFlatExprCommandT m a
  = GraphSliceSendFlatExprCommandT { run :: StateT TState (ReaderT TEnv (StructsT (InnerT m))) a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance MonadTrans T where
    lift = liftInner . lift

instance Monad m => MonadInner (InnerT m) (T m) where
    liftInner = GraphSliceSendFlatExprCommandT . lift . lift . lift

instance MonadMapBase T where
    mapBase f g = #run %~ (mapStateT (mapReaderT (mapStructsT (mapBase f g))))

liftStructs :: Monad m => StructsT (InnerT m) a -> T m a
liftStructs = GraphSliceSendFlatExprCommandT . lift . lift

liftPure :: Monad m => StateT TState (Reader TEnv) a -> T m a
liftPure = GraphSliceSendFlatExprCommandT . mapStateT (mapReaderT (return . runIdentity))

send :: C m => SolverExprCommand -> T m ()
send = liftInner . sendSolverExprCommand

runGraphSliceSendFlatExprCommandTStep :: Monad m => (Ident -> Struct) -> [SolverExpr] -> T m a -> InnerT m a
runGraphSliceSendFlatExprCommandTStep lookupStruct rodataPtrs =
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
      , exprMap :: Map Ident ExtendedExpr
      , exprCache :: Map SolverExpr SolverExpr
      , impliesStackEqCache :: Map ImpliesStackEqCacheKey SolverExpr
      , pvalids :: Map Ident (Map PValidKey SolverExpr)
      , modelVars :: Set Ident
      , modelExprs :: Map SolverExpr SolverExpr
      }
  deriving (Generic)

data ImpliesStackEqCacheKey
  = ImpliesStackEqCacheKey
      { sp :: SolverExpr
      , stack1 :: ExtendedExpr
      , stack2 :: ExtendedExpr
      }
  deriving (Eq, Generic, Ord)

data PValidKey
  = PValidKey
      { pvKind :: PValidKind
      , pvTy :: PValidType SolverExprContext
      , ptr :: SolverExpr
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

data ExtendedExpr
  = ExtendedExprExpr SolverExpr
  | ExtendedExprHtd HtdExpr
  | ExtendedExprSplitMem SplitMemExpr
  deriving (Eq, Generic, Ord, Show)

data HtdExpr
  = HtdExprIdent Ident
  | HtdExprIfThenElse SolverExpr HtdExpr HtdExpr
  deriving (Eq, Generic, Ord, Show)

data SplitMemExpr
  = SplitMemExpr
      { split :: SolverExpr
      , top :: SolverExpr
      , bottom :: SolverExpr
      }
  deriving (Eq, Generic, Ord, Show)

--

withMapSlot :: (C m, Ord k) => Lens' TState (M.Map k v) -> k -> T m v -> T m v
withMapSlot = withMapSlotWith $ liftPure . mapStateT (return . runIdentity)

type NameHint = String

takeFreshName :: C m => NameHint -> T m Ident
takeFreshName nameHint = liftPure $ zoom #names $ takeFreshNameWith Ident nameHint

askRODataPtrs :: C m => T m [SolverExpr]
askRODataPtrs = liftPure $ gview #rodataPtrs

--

isTypeRepresentable :: ExprType -> Bool
isTypeRepresentable = \case
    ExprTypeWord _ -> True
    ExprTypeBool -> True
    ExprTypeToken -> True
    _ -> False

addVar :: C m => NameHint -> ExprType -> T m SolverExpr
addVar nameHint ty = do
    name <- takeFreshName nameHint
    let var = NameTy name ty
    send $ ExprCommandDeclare var
    when (isTypeRepresentable ty) $ do
        liftPure $ #modelVars %= S.insert name
    return $ varFromNameTyE var

addDef :: C m => NameHint -> SolverExpr -> T m SolverExpr
addDef = addDefWithInlineHint ExprCommandInlineHintNever

addDefWithInlineHint :: C m => ExprCommandInlineHint -> NameHint -> SolverExpr -> T m SolverExpr
addDefWithInlineHint inline nameHint expr = do
    name <- takeFreshName nameHint
    let var = NameTy name expr.ty
    send $ ExprCommandDefine inline var expr
    when (isTypeRepresentable expr.ty) $ do
        liftPure $ #modelVars %= S.insert name
    return $ varFromNameTyE var

addExtendedDefWithInlineHint :: C m => ExprCommandInlineHint -> NameHint -> ExtendedExpr -> T m ExtendedExpr
addExtendedDefWithInlineHint inline nameHint = \case
    ExtendedExprExpr expr -> ExtendedExprExpr <$> do
        addDefWithInlineHint inline nameHint expr
    ExtendedExprHtd htd -> ExtendedExprHtd <$> do
        return htd
    ExtendedExprSplitMem splitMem -> ExtendedExprSplitMem <$> do
        let f suffix expr = addDef (nameHint ++ "_" ++ suffix) expr
        split <- f "split" splitMem.split
        top <- f "top" splitMem.top
        bottom <- f "bottom" splitMem.bottom
        return $ SplitMemExpr
            { split
            , top
            , bottom
            }

cacheExpr :: C m => NameHint -> SolverExpr -> T m SolverExpr
cacheExpr = cacheExprWithInlineHint ExprCommandInlineHintNever

cacheExprInline :: C m => NameHint -> SolverExpr -> T m SolverExpr
cacheExprInline = cacheExprWithInlineHint ExprCommandInlineHintSometimes

cacheExprWithInlineHint :: C m => ExprCommandInlineHint -> NameHint -> SolverExpr -> T m SolverExpr
cacheExprWithInlineHint inline nameHint expr = case expr.value of
    ExprValueVar _ -> return expr
    _ -> withMapSlot #exprCache expr $ addDefWithInlineHint inline nameHint expr

cachePtr :: C m => SolverExpr -> T m SolverExpr
cachePtr = cacheExpr "ptr"

noteModelExpr :: C m => SolverExpr -> T m ()
noteModelExpr expr = void $ withMapSlot #modelExprs expr $ addDef "query" expr

maybeNoteModelExpr :: C m => SolverExpr -> [SolverExpr] -> T m ()
maybeNoteModelExpr expr subexprs =
    when (isTypeRepresentable expr.ty && not (all (isTypeRepresentable . (.ty)) subexprs)) $ do
        noteModelExpr expr

assertSolverExpr :: C m => SolverExpr -> T m ()
assertSolverExpr = send . ExprCommandAssert

sendAccumulatedAssertionsInner :: C m => T m ()
sendAccumulatedAssertionsInner = do
    sendPValidDomAssertions

--

sendFlatExprCommand :: C m => FlatExprCommand -> T m ()
sendFlatExprCommand = \case
    ExprCommandDeclare origVar -> case origVar.ty of
        ExprTypePms -> return ()
        _ -> do
            val <- case origVar.ty of
                ExprTypeHtd -> addHtd origVar.name.unwrap
                _ -> ExtendedExprExpr <$> addVar origVar.name.unwrap origVar.ty
            liftPure $ #exprMap %= M.insertWith undefined origVar.name val
    ExprCommandDefine inlineHint origVar origVal -> case origVar.ty of
        ExprTypePms -> return ()
        _ -> do
            val <- convertFlatExprExtended origVal
                >>= addExtendedDefWithInlineHint inlineHint origVar.name.unwrap
            liftPure $ #exprMap %= M.insertWith undefined origVar.name val
    ExprCommandAssert expr -> assertSolverExpr =<< convertFlatExpr expr

convertFlatExprExtended :: C m => FlatExpr -> T m ExtendedExpr
convertFlatExprExtended expr = case expr.value of
    ExprValueVar name -> liftPure $ use $ #exprMap % expectingAt name
    ExprValueOp op args -> traverse convertFlatExprExtended args >>= convertFlatOpExpr expr.ty op
    _ -> return $ ExtendedExprExpr $ castExpr expr

convertFlatExpr :: C m => FlatExpr -> T m SolverExpr
convertFlatExpr expr = viewExpecting #_ExtendedExprExpr <$> convertFlatExprExtended expr

convertFlatOpExpr :: C m => ExprType -> Op -> [ExtendedExpr] -> T m ExtendedExpr
convertFlatOpExpr exprTy op args = do
    expr <- case (op, args) of
        (OpIfThenElse, ~[ExtendedExprExpr cond, x, y]) ->
            return $ convertIfThenElse cond x y
        (OpExt OpExtSplitMem, ~[ExtendedExprExpr split, ExtendedExprExpr top, ExtendedExprExpr bottom]) ->
            return $ ExtendedExprSplitMem $ SplitMemExpr
                { split
                , top
                , bottom
                }
        (OpMemUpdate, ~[m, ExtendedExprExpr p, ExtendedExprExpr v]) ->
            convertMemUpdate m p v v.ty
        (OpMemAcc, ~[m, ExtendedExprExpr p]) ->
            ExtendedExprExpr <$> convertMemAccess exprTy m p
        (OpExt OpExtStackEqualsImplies, ~[ExtendedExprExpr sp1, stack1, ExtendedExprExpr sp2, stack2]) ->
            ExtendedExprExpr <$> convertStackEqualsImplies sp1 stack1 sp2 stack2
        (OpExt OpExtImpliesStackEquals, ~[ExtendedExprExpr sp1, stack1, ExtendedExprExpr sp2, stack2]) ->
            ExtendedExprExpr <$> convertImpliesStackEquals sp1 stack1 sp2 stack2
        (OpPArrayValid, ~[ExtendedExprHtd htd, ExtendedExprExpr tyExpr, ExtendedExprExpr ptrExpr, ExtendedExprExpr len]) ->
            ExtendedExprExpr <$> do
                let ExprValueType tyVal = tyExpr.value
                ptr <- cachePtr ptrExpr
                addPValidExpr htd $ PValidKey
                    { pvKind = pvalidKindFromOp op
                    , pvTy = PValidTypeArray { ty = tyVal, len }
                    , ptr
                    }
        (_, ~[ExtendedExprHtd htd, ExtendedExprExpr tyExpr, ExtendedExprExpr ptrExpr])
                | op `elem` [OpPValid, OpPGlobalValid, OpPWeakValid] ->
            ExtendedExprExpr <$> do
                let ExprValueType tyVal = tyExpr.value
                ptr <- cachePtr ptrExpr
                addPValidExpr htd $ PValidKey
                    { pvKind = pvalidKindFromOp op
                    , pvTy = PValidTypeType tyVal
                    , ptr
                    }
        (OpHtdUpdate, _) ->
            addHtd "update_htd"
        _ ->
            return $ ExtendedExprExpr $ Expr exprTy $ ExprValueOp op $ map (viewExpecting #_ExtendedExprExpr) args
    sequenceA_ $
        maybeNoteModelExpr
            <$> preview #_ExtendedExprExpr expr
            <*> traverse (preview #_ExtendedExprExpr) args
    return expr

convertIfThenElse :: SolverExpr -> ExtendedExpr -> ExtendedExpr -> ExtendedExpr
convertIfThenElse cond xExt yExt = case (xExt, yExt) of
    (ExtendedExprExpr x, ExtendedExprExpr y) -> ExtendedExprExpr $ ite x y
    (ExtendedExprHtd x, ExtendedExprHtd y) -> ExtendedExprHtd $ HtdExprIfThenElse cond x y
    (x, y) ->
        let xs = trivSplit x
            ys = trivSplit y
         in ExtendedExprSplitMem $ SplitMemExpr
                { split =
                    if xs.split == ys.split
                    then xs.split
                    else (ite `on` (.split)) xs ys
                , top = (ite `on` (.top)) xs ys
                , bottom = (ite `on` (.bottom)) xs ys
                }
  where
    ite = ifThenElseE cond
    trivSplit = \case
        ExtendedExprSplitMem splitMem -> splitMem
        ExtendedExprExpr expr -> SplitMemExpr
            { split = machineWordE 0
            , top = expr
            , bottom = expr
            }

convertMemUpdate :: C m => ExtendedExpr -> SolverExpr -> SolverExpr -> ExprType -> T m ExtendedExpr
convertMemUpdate extExpr p v ty = case extExpr of
    ExtendedExprExpr mem -> ExtendedExprExpr <$> convertMemUpdateSimpleExpr mem p v ty
    ExtendedExprSplitMem splitMem -> ExtendedExprSplitMem <$> do
        p' <- cacheExprInline "memupd_pointer" p
        v' <- cacheExprInline "memupd_val" v
        top <- cacheExprInline "split_mem_top" splitMem.top
        bottom <- cacheExprInline "split_mem_bot" splitMem.bottom
        topUpd <- convertMemUpdateSimpleExpr top p' v' ty
        bottomUpd <- convertMemUpdateSimpleExpr bottom p' v' ty
        let f = ifThenElseE (splitMem.split `lessEqE` p')
        return $ SplitMemExpr
            { split = splitMem.split
            , top = f topUpd top
            , bottom = f bottom bottomUpd
            }

convertMemUpdateSimpleExpr :: C m => SolverExpr -> SolverExpr -> SolverExpr -> ExprType -> T m SolverExpr
convertMemUpdateSimpleExpr mem p v ty@(ExprTypeWord bits) = do
    case bits of
        8 -> do
            let aligned = p `bitwiseAndE` word32E 0xfffffffd
            noteModelExpr aligned
            noteModelExpr $ memAccE word32T aligned mem
        _ -> do
            noteModelExpr p
            noteModelExpr $ memAccE ty p mem
    return $ memUpdE p mem v

convertMemAccess :: C m => ExprType -> ExtendedExpr -> SolverExpr -> T m SolverExpr
convertMemAccess ty@(ExprTypeWord _) extExpr p = case extExpr of
    ExtendedExprExpr mem -> do
        let v = memAccE ty p mem
        noteModelExpr p
        noteModelExpr v
        return v
    ExtendedExprSplitMem splitMem -> do
        p' <- cacheExprInline "memacc_pointer" p
        let f side = convertMemAccess ty (ExtendedExprExpr (side splitMem)) p'
        ifThenElseE (splitMem.split `lessEqE` p') <$> f (.top) <*> f (.bottom)

convertStackEqualsImplies :: C m => SolverExpr -> ExtendedExpr -> SolverExpr -> ExtendedExpr -> T m SolverExpr
convertStackEqualsImplies sp1 stack1 sp2 stack2 =
    if sp1 == sp2 && stack1 == stack2
    then return trueE
    else do
        let ExtendedExprSplitMem splitMem2 = stack2
        ensureM $ splitMem2.split == sp2
        let eq = case stack1 of
                ExtendedExprExpr s ->
                    splitMem2.top `eqE` s
                ExtendedExprSplitMem splitMem1 ->
                    (splitMem1.split `lessEqE` splitMem2.split)
                        `impliesE` (splitMem2.top `eqE` splitMem1.top)
        return $ (sp1 `eqE` sp2) `andE` eq

convertImpliesStackEquals :: C m => SolverExpr -> ExtendedExpr -> SolverExpr -> ExtendedExpr -> T m SolverExpr
convertImpliesStackEquals sp1 stack1 sp2 stack2 = do
    let key = ImpliesStackEqCacheKey
            { sp = sp1
            , stack1
            , stack2
            }
    eq <- withMapSlot #impliesStackEqCache key $ do
        addr <- addVar "stack-eq-witness" word32T
        assertSolverExpr $ (addr `bitwiseAndE` machineWordE 3) `eqE` machineWordE 0
        assertSolverExpr $ key.sp `lessEqE` addr
        let f mem = convertMemAccess word32T mem addr
        solverExpr <- eqE <$> f key.stack1 <*> f key.stack2
        addDef "stack-eq" solverExpr
    return $ (sp1 `eqE` sp2) `andE` eq

addHtd :: C m => NameHint -> T m ExtendedExpr
addHtd nameHint = do
    name <- takeFreshName nameHint
    liftPure $ #pvalids %= M.insertWith undefined name M.empty
    return $ ExtendedExprHtd (HtdExprIdent name)

addPValidExpr :: C m => HtdExpr -> PValidKey -> T m SolverExpr
addPValidExpr = go
  where
    go htd key = case htd of
        HtdExprIdent name ->
            addPValid name key
        HtdExprIfThenElse cond l r ->
            ifThenElseE cond
                <$> go l key
                <*> go r key

addPValid :: C m => Ident -> PValidKey -> T m SolverExpr
addPValid htd key = do
    others <- liftPure $ use $ #pvalids % expectingAt htd
    when (M.null others) $ addRODataPValids htd
    withMapSlot (#pvalids % expectingAt htd) key $ do
        var <- addVar "pvalid" boolT
        let info = mkPvInfo key var
        do  fact <- liftStructs $ alignValidIneq info.pvTy info.ptr
            assertSolverExpr $ info.pv `impliesE` fact
        for_ (M.toList others) $ \(otherKey, otherVar) -> do
            let otherInfo = mkPvInfo otherKey otherVar
            let pvKinds = [otherInfo.pvKind, info.pvKind]
            unless (PValidKindPWeakValid `elem` pvKinds && PValidKindPGlobalValid `notElem` pvKinds) $ do
                let applyAssertion f = do
                        fact <- liftStructs $ f info otherInfo
                        assertSolverExpr fact
                applyAssertion pvalidAssertion1
                applyAssertion pvalidAssertion2
        return var
  where
    mkPvInfo (PValidKey { pvKind, pvTy, ptr }) pv = PValidInfo
        { pvKind
        , pvTy
        , ptr
        , pv
        }

addRODataPValids :: C m => Ident -> T m ()
addRODataPValids htd = do
    rodataPtrs <- askRODataPtrs
    for_ rodataPtrs $ \(Expr (ExprTypePtr roTy) (ExprValueNum roAddr)) -> do
        ptr <- cachePtr $ machineWordE roAddr
        let key = PValidKey
                { pvKind = PValidKindPGlobalValid
                , pvTy = PValidTypeType roTy
                , ptr
                }
        assertSolverExpr =<< addPValid htd key

sendPValidDomAssertions :: C m => T m ()
sendPValidDomAssertions = do
    ensureM cheatMemDoms
    return ()
