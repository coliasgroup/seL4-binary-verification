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
import Data.Foldable (for_)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%%=), (%=))

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

instance MonadLiftInner InnerT T where
    liftInner = GraphSliceSendFlatExprCommandT . lift . lift . lift

instance MonadMapInnermost T where
    mapInnermost f = #run %~ mapStateT (mapReaderT (mapStructsT (mapInnermost f)))

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
      , cache :: Map SolverExpr SolverExpr
      , flatExprVars :: Map Ident ExtendedExpr
      , tokens :: Map Ident SolverExpr
      , pvalids :: Map Ident (Map PValidKey SolverExpr)
      , impliesStackEqCache :: Map ImpliesStackEqCacheKey SolverExpr
      , deferredSplitVars :: Map Ident (Maybe SolverExpr)
      , lhsSplitVars :: Set Ident
      }
  deriving (Generic)

data PValidKey
  = PValidKey
      { pvKind :: PValidKind
      , pvTy :: PValidType SolverExprContext
      , ptr :: SolverExpr
      }
  deriving (Eq, Generic, Ord)

data ImpliesStackEqCacheKey
  = ImpliesStackEqCacheKey
      { sp :: SolverExpr
      , stack1 :: ExtendedExpr
      , stack2 :: ExtendedExpr
      }
  deriving (Eq, Generic, Ord)

initEnv :: [SolverExpr] -> TEnv
initEnv rodataPtrs = TEnv
    { rodataPtrs
    }

initState :: TState
initState = TState
    { names = S.empty
    , cache = M.empty
    , flatExprVars = M.empty
    , tokens = M.empty
    , pvalids = M.empty
    , impliesStackEqCache = M.empty
    , deferredSplitVars = M.empty
    , lhsSplitVars = S.empty
    }

--

data ExtendedExpr
  = ExtendedExprExpr SolverExpr
  | ExtendedExprToken SolverExpr
  | ExtendedExprPms
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
      , deps :: Set Ident
      }
  deriving (Eq, Generic, Ord, Show)

--

withMapSlot :: (C m, Ord k) => Lens' TState (M.Map k v) -> k -> T m v -> T m v
withMapSlot = withMapSlotWith $ liftPure . mapStateT (return . runIdentity)

type NameHint = String

takeFreshName :: C m => NameHint -> T m Ident
takeFreshName nameHint = liftPure $ zoom #names $ takeFreshNameWith Ident nameHint

--

addVar :: C m => NameHint -> ExprType -> T m SolverExpr
addVar nameHint ty = do
    name <- takeFreshName nameHint
    let var = NameTy name ty
    send $ ExprCommandDeclare var
    return $ varFromNameTyE var

addDef :: C m => NameHint -> SolverExpr -> T m SolverExpr
addDef = addDefWithInlineHint ExprCommandInlineHintNever

addDefWithInlineHint :: C m => ExprCommandInlineHint -> NameHint -> SolverExpr -> T m SolverExpr
addDefWithInlineHint inline nameHint expr = do
    name <- takeFreshName nameHint
    let var = NameTy name expr.ty
    send $ ExprCommandDefine inline var expr
    return $ varFromNameTyE var

addExtendedDefWithInlineHint :: C m => ExprCommandInlineHint -> NameHint -> ExtendedExpr -> T m ExtendedExpr
addExtendedDefWithInlineHint inline nameHint = \case
    ExtendedExprExpr expr -> ExtendedExprExpr <$> do
        addDefWithInlineHint inline nameHint expr
    ExtendedExprToken expr -> ExtendedExprToken <$> do
        addDefWithInlineHint inline nameHint expr
    ExtendedExprSplitMem expr -> ExtendedExprSplitMem <$> do
        addSplitMemDef nameHint expr
    extExpr -> return extExpr

cacheExpr :: C m => NameHint -> SolverExpr -> T m SolverExpr
cacheExpr = cacheExprWithInlineHint ExprCommandInlineHintNever

cacheExprInline :: C m => NameHint -> SolverExpr -> T m SolverExpr
cacheExprInline = cacheExprWithInlineHint ExprCommandInlineHintSometimes

cacheExprWithInlineHint :: C m => ExprCommandInlineHint -> NameHint -> SolverExpr -> T m SolverExpr
cacheExprWithInlineHint inline nameHint expr = case expr.value of
    ExprValueVar _ -> return expr
    _ -> withMapSlot #cache expr $ addDefWithInlineHint inline nameHint expr

cachePtr :: C m => SolverExpr -> T m SolverExpr
cachePtr = cacheExpr "ptr"

assertSolverExpr :: C m => SolverExpr -> T m ()
assertSolverExpr = send . ExprCommandAssert

sendAccumulatedAssertionsInner :: C m => T m ()
sendAccumulatedAssertionsInner = do
    ensureNoUnconstrainedSplitVars
    sendPValidDomAssertions

concreteTokenType :: ExprType
concreteTokenType = ExprTypeWord 64

--

sendFlatExprCommand :: C m => FlatExprCommand -> T m ()
sendFlatExprCommand = \case
    ExprCommandDeclare origVar -> do
        val <- case origVar.ty of
            ExprTypeStack -> ExtendedExprSplitMem <$> addSplitMemVar origVar.name.unwrap
            ExprTypeToken -> ExtendedExprToken <$> addVar origVar.name.unwrap concreteTokenType
            ExprTypePms -> return ExtendedExprPms
            ExprTypeHtd -> addHtd origVar.name.unwrap
            _ -> ExtendedExprExpr <$> addVar origVar.name.unwrap origVar.ty
        liftPure $ #flatExprVars %= M.insertWith undefined origVar.name val
    ExprCommandDefine inlineHint origVar origVal -> do
        val <- convertFlatExprExtended origVal
            >>= addExtendedDefWithInlineHint inlineHint origVar.name.unwrap
        liftPure $ #flatExprVars %= M.insertWith undefined origVar.name val
    ExprCommandAssert expr -> assertSolverExpr =<< convertFlatExpr expr

addSplitMemVar :: C m => NameHint -> T m SplitMemExpr
addSplitMemVar nameHint = do
    let f suffix = addVar (nameHint ++ "_" ++ suffix)
    split <- f "split_deferred" machineWordT
    top <- f "top" memT
    bottom <- f "bottom" memT
    let splitName = nameFromVarE split -- TODO clunky
    liftPure $ #deferredSplitVars %= M.insertWith undefined splitName Nothing
    return $ SplitMemExpr
        { split
        , top
        , bottom
        , deps = S.singleton (nameFromVarE split) -- TODO clunky
        }

addSplitMemDef :: C m => NameHint -> SplitMemExpr -> T m SplitMemExpr
addSplitMemDef nameHint splitMem = do
    let f suffix = addDef (nameHint ++ "_" ++ suffix)
    split <- f "split" splitMem.split
    top <- f "top" splitMem.top
    bottom <- f "bottom" splitMem.bottom
    return $ SplitMemExpr
        { split
        , top
        , bottom
        , deps = splitMem.deps
        }

ensureNoUnconstrainedSplitVars :: C m => T m ()
ensureNoUnconstrainedSplitVars = do
    lhsSplitVars <- liftPure $ use #lhsSplitVars
    deferredSplitVars <- liftPure $ use #deferredSplitVars
    let missing = S.intersection lhsSplitVars $ M.keysSet $ M.filter isNothing deferredSplitVars
    ensureM $ S.null missing

convertFlatExprExtended :: C m => FlatExpr -> T m ExtendedExpr
convertFlatExprExtended expr = case expr.value of
    ExprValueVar name -> liftPure $ use $ #flatExprVars % expectingAt name
    ExprValueToken tok -> ExtendedExprExpr <$> convertToken tok
    ExprValueOp op args -> traverse convertFlatExprExtended args >>= convertFlatOpExpr expr.ty op
    _ -> return $ ExtendedExprExpr $ castExpr expr

convertFlatExpr :: C m => FlatExpr -> T m SolverExpr
convertFlatExpr expr = viewExpecting #_ExtendedExprExpr <$> convertFlatExprExtended expr

convertFlatOpExpr :: C m => ExprType -> Op -> [ExtendedExpr] -> T m ExtendedExpr
convertFlatOpExpr exprTy op args =
    case (op, args) of
        (OpIfThenElse, ~[ExtendedExprExpr cond, x, y]) ->
            return $ convertIfThenElse cond x y
        (OpExt OpExtMarkedStack, ~[stack]) ->
            return stack
        (OpMemUpdate, ~[m, ExtendedExprExpr p, ExtendedExprExpr v]) ->
            convertMemUpdate m p v
        (OpMemAcc, ~[m, ExtendedExprExpr p]) ->
            ExtendedExprExpr <$> convertMemAccess exprTy m p
        (OpExt OpExtStackEqualsImplies,
                ~[ ExtendedExprExpr sp1
                 , stack1
                 , ExtendedExprExpr sp2
                 , ExtendedExprSplitMem stack2
                 ]) ->
            ExtendedExprExpr <$> convertStackEqualsImplies sp1 stack1 sp2 stack2
        (OpExt OpExtImpliesStackEquals,
                ~[ ExtendedExprExpr sp1
                 , stack1
                 , ExtendedExprExpr sp2
                 , stack2
                 ]) ->
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
        (OpMemDom, ~[ExtendedExprExpr _p, ExtendedExprExpr _dom]) -> do
            ensureM cheatMemDoms
            return $ ExtendedExprExpr trueE
        _ ->
            return $ ExtendedExprExpr $
                Expr exprTy $ ExprValueOp op $ map (viewExpecting #_ExtendedExprExpr) args

convertIfThenElse :: SolverExpr -> ExtendedExpr -> ExtendedExpr -> ExtendedExpr
convertIfThenElse cond xExt yExt = case (xExt, yExt) of
    (ExtendedExprExpr x, ExtendedExprExpr y) -> ExtendedExprExpr $ ite x y
    (ExtendedExprToken x, ExtendedExprToken y) -> ExtendedExprToken $ ite x y
    (ExtendedExprPms, ExtendedExprPms) -> ExtendedExprPms
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
                , deps = xs.deps <> ys.deps
                }
  where
    ite = ifThenElseE cond
    trivSplit = \case
        ExtendedExprSplitMem splitMem -> splitMem
        ExtendedExprExpr expr -> SplitMemExpr
            { split = machineWordE 0
            , top = expr
            , bottom = expr
            , deps = S.empty
            }

convertMemUpdate :: C m => ExtendedExpr -> SolverExpr -> SolverExpr -> T m ExtendedExpr
convertMemUpdate extExpr p v = case extExpr of
    ExtendedExprExpr mem -> return $ ExtendedExprExpr $ memUpdE p mem v
    ExtendedExprSplitMem splitMem -> ExtendedExprSplitMem <$> do
        p' <- cacheExprInline "memupd_pointer" p
        v' <- cacheExprInline "memupd_val" v
        top <- cacheExprInline "split_mem_top" splitMem.top
        bottom <- cacheExprInline "split_mem_bot" splitMem.bottom
        let upd mem = memUpdE p' mem v'
        let f = ifThenElseE (splitMem.split `lessEqE` p')
        return $ SplitMemExpr
            { split = splitMem.split
            , top = f (upd top) top
            , bottom = f bottom (upd bottom)
            , deps = splitMem.deps
            }

convertMemAccess :: C m => ExprType -> ExtendedExpr -> SolverExpr -> T m SolverExpr
convertMemAccess ty extExpr p = case extExpr of
    ExtendedExprExpr mem -> return $ memAccE ty p mem
    ExtendedExprSplitMem (SplitMemExpr { split, top, bottom }) -> do
        p' <- cacheExprInline "memacc_pointer" p
        let acc = memAccE ty p'
        return $ ifThenElseE (split `lessEqE` p') (acc top) (acc bottom)

defineDeferredSplitVar :: C m => SolverExpr -> SolverExpr -> T m ()
defineDeferredSplitVar sp split = do
    let splitName = nameFromVarE split
    prevOpt <- liftPure $ #deferredSplitVars % expectingAt splitName %%=
        \curOpt -> (curOpt, Just sp)
    for_ prevOpt $ \prev -> do
        ensureM $ prev == sp
    assertSolverExpr $ split `eqE` sp

ensureRHSNotInLHSDeps :: C m => Ident -> ExtendedExpr -> T m ()
ensureRHSNotInLHSDeps split2 = \case
    ExtendedExprSplitMem stack1 -> do
        ensureM $ split2 `S.notMember` stack1.deps
    _ -> return ()

convertStackEqualsImplies :: C m => SolverExpr -> ExtendedExpr -> SolverExpr -> SplitMemExpr -> T m SolverExpr
convertStackEqualsImplies sp1 stack1 sp2 stack2 = do
    defineDeferredSplitVar sp2 stack2.split
    ensureRHSNotInLHSDeps (nameFromVarE stack2.split) stack1
    return $
        if sp1 == sp2 && stack1 == ExtendedExprSplitMem stack2
        then trueE
        else
            let eq = case stack1 of
                    ExtendedExprExpr stack1Expr ->
                        stack2.top `eqE` stack1Expr
                    ExtendedExprSplitMem stack1Split ->
                        (stack1Split.split `lessEqE` stack2.split)
                            `impliesE` (stack1Split.top `eqE` stack2.top)
             in (sp1 `eqE` sp2) `andE` eq

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
        let acc stack = convertMemAccess word32T stack addr
        solverExpr <- eqE <$> acc key.stack1 <*> acc key.stack2
        addDef "stack-eq" solverExpr
    return $ (sp1 `eqE` sp2) `andE` eq

convertToken :: C m => Ident -> T m SolverExpr
convertToken tok = withMapSlot #tokens tok $ do
    n <- liftPure $ use $ #tokens % to M.size
    addDef
        ("token_" ++ tok.unwrap)
        (numE concreteTokenType (toInteger n))

addHtd :: C m => NameHint -> T m ExtendedExpr
addHtd nameHint = do
    name <- takeFreshName nameHint
    liftPure $ #pvalids %= M.insertWith undefined name M.empty
    return $ ExtendedExprHtd $ HtdExprIdent name

addPValidExpr :: C m => HtdExpr -> PValidKey -> T m SolverExpr
addPValidExpr = go
  where
    go htd key = case htd of
        HtdExprIdent name ->
            addPValidAndInit name key
        HtdExprIfThenElse cond l r ->
            ifThenElseE cond
                <$> go l key
                <*> go r key

addPValidAndInit :: C m => Ident -> PValidKey -> T m SolverExpr
addPValidAndInit htd key = do
    others <- liftPure $ use $ #pvalids % expectingAt htd
    when (M.null others) $ addRODataPValids htd
    addPValid htd key

addPValid :: C m => Ident -> PValidKey -> T m SolverExpr
addPValid htd key = do
    others <- liftPure $ use $ #pvalids % expectingAt htd
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
    rodataPtrs <- liftPure $ gview #rodataPtrs
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
