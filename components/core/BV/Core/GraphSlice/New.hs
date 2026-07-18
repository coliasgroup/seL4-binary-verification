{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.GraphSlice.New
    ( AsmRefineGraphSliceInput (..)
    , ExprEnv
    , FlatExpr
    , GraphSliceHooks
    , GraphSliceInput (..)
    , GraphSliceT
    , MonadGraphSliceSendSExpr (..)
    , PcEnv (..)
    , addAccumulatedAssertions
    , askProblemWithAnalysis
    , asmRefineGraphSliceHooks
    , assertExpr
    , compileProofCheckGroup
    , convertExpr
    , defaultGraphSliceHooks
    , flattenExpr
    , getAllPcEnvs
    , getCallOrderCompat
    , getCallsByFunName
    , getInductVar
    , getPc
    , getPcEnv
    , getSuccessVar
    , interpretCheck
    , interpretHyp
    , interpretHypImps
    , isVisitOk
    , mapGraphSliceT
    , runAsmRefineGraphSliceT
    , runGraphSliceT
    , withAsmStackSplitting
    , withConstRetAssumptions
    , withFast
    ) where

import BV.Core.GraphSlice.New.Common
import BV.Core.GraphSlice.New.Flat
import BV.Core.GraphSlice.New.Flatten
import BV.Core.GraphSlice.New.SendFlatExprCommand
import BV.Core.GraphSlice.New.SendSolverExprCommand

import BV.Core.Logic (eqHandlingRelWrapper, strengthenHyp)
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Utils (ensure)

import Control.Monad ((>=>))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), hoistMaybe, runMaybeT)
import Control.Monad.Writer (runWriter)
import Data.Either (isRight)
import Data.Foldable (toList)
import Data.List (genericIndex)
import qualified Data.Map as M
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics

data GraphSliceInput t
  = GraphSliceInput
      { structs :: ByTag t (M.Map Ident Struct)
      , rodata :: ROData
        -- TODO rename
      , pwa :: ProblemWithAnalysis t
      }
  deriving (Generic)

runGraphSliceT
    :: (Tag t, MonadGraphSliceSendSExpr m)
    => GraphSliceHooks t
    -> GraphSliceInput t
    -> GraphSliceT t m a
    -> m a
runGraphSliceT hooks input =
      runGraphSliceSendSolverExprCommandTStep input.rodata
    . runGraphSliceSendFlatExprCommandTStep structs (rodataPtrsFromROData input.rodata)
    . runGraphSliceFlatTStep
    . runGraphSliceTStep input.pwa hooks
  where
    -- TODO scope structs with tag and use `M.unionsWith undefined`
    structs = (M.!) $ M.unionsWith ensureEq $
        rodataStructsOf input.rodata : toList input.structs
    ensureEq x y = ensure (x == y) x

rodataPtrsFromROData :: ROData -> [Expr c]
rodataPtrsFromROData rodata =
    [ pointerE (structT structName) (machineWordE range.addr)
    | (structName, range) <- rodataStructNamesOf rodata
    ]

data AsmRefineGraphSliceInput
  = AsmRefineGraphSliceInput
      { repGraphInput :: GraphSliceInput AsmRefineTag
      , lookupSig :: LookupFunctionSignature AsmRefineTag
      , pairings :: Pairings'
      }
  deriving (Generic)

runAsmRefineGraphSliceT
    :: MonadGraphSliceSendSExpr m
    => AsmRefineGraphSliceInput
    -> GraphSliceT AsmRefineTag m a
    -> m a
runAsmRefineGraphSliceT input = runGraphSliceT hooks input.repGraphInput
  where
    hooks = asmRefineGraphSliceHooks input.lookupSig input.pairings undefined

--

asmRefineGraphSliceHooks
    :: t ~ AsmRefineTag
    => LookupFunctionSignature t
    -> Pairings t
    -> ArgRenames t
    -> GraphSliceHooks t
asmRefineGraphSliceHooks lookupSig pairings argRenames =
    withAsmStackSplitting lookupSig argRenames $
        withAddFunAsserts lookupSig pairings $
            defaultGraphSliceHooks

withAsmStackSplitting
    :: HasTagIsAsm t
    => LookupFunctionSignature t
    -> ArgRenames t
    -> GraphSliceHooks t
    -> GraphSliceHooks t
withAsmStackSplitting lookupSig _argRenames = withIsStack $ asmRefineIsStackHook lookupSig

asmRefineIsStackHook :: HasTagIsAsm t => LookupFunctionSignature t -> WithTag t Ident -> FunctionSignatureDirection -> Integer -> Bool
asmRefineIsStackHook lookupSig fun direction i =
    tagIsAsm fun.tag &&
        genericIndex (viewFunctionSignatureDirection direction (lookupSig fun)) i == asmStackVar
  where
    asmStackVar = NameTy (Ident "stack") memT

--

mapGraphSliceT
    :: (forall a. m a -> n a)
    -> GraphSliceT t m b
    -> GraphSliceT t n b
mapGraphSliceT = mapInnermost

assertExpr :: (Tag t, MonadGraphSliceSendSExpr m) => FlatExpr -> GraphSliceT t m ()
assertExpr = liftInner . assertFlatExpr

convertExpr :: (Tag t, MonadGraphSliceSendSExpr m) => FlatExpr -> GraphSliceT t m SExprWithPlaceholders
convertExpr =
    liftInner . liftInner . convertFlatExpr
        >=> liftInner . liftInner . liftInner . convertSolverExpr

addAccumulatedAssertions :: (Tag t, MonadGraphSliceSendSExpr m) => GraphSliceT t m ()
addAccumulatedAssertions = liftInner $ liftInner $ sendAccumulatedAssertionsInner

--

normalizeVisitM
    :: (Tag t, Monad m)
    => Visit t
    -> GraphSliceT t m (Either VisitTooGeneral (Maybe (NormalizedVisit t)))
normalizeVisitM visit = normalizeVisit visit <$> askProblemWithAnalysis

isVisitOk :: (Tag t, Monad m) => Visit t -> GraphSliceT t m Bool
isVisitOk visit = isRight <$> normalizeVisitM visit

getPcEnv :: (Tag t, MonadGraphSliceSendSExpr m) => Visit t -> GraphSliceT t m (Maybe PcEnv)
getPcEnv visit = runMaybeT $ do
    r <- lift $ normalizeVisitM visit
    let Right normOpt = r
    norm <- hoistMaybe normOpt
    MaybeT $ getPcEnvNorm norm

getPc :: (Tag t, MonadGraphSliceSendSExpr m) => Visit t -> GraphSliceT t m FlatExpr
getPc visit = getPcEnv visit <&> \case
    Just (PcEnv pc _) -> pc
    Nothing -> falseE

getAllPcEnvs :: (Tag t, Monad m) => GraphSliceT t m [(Visit t, PcEnv)]
getAllPcEnvs = over (traversed % _1) unwrapNormalizedVisit <$> getAllPcEnvsNorm

getCallsByFunName :: (Tag t, Monad m) => GraphSliceT t m (M.Map (WithTag t Ident) [Visit t])
getCallsByFunName = (fmap . map) unwrapNormalizedVisit <$> getCallsByFunNameNorm

getCallOrderCompat :: (Tag t, Monad m) => GraphSliceT t m [Visit t]
getCallOrderCompat = map unwrapNormalizedVisit <$> getCallOrderCompatNorm

getSuccessVar :: (Tag t, MonadGraphSliceSendSExpr m) => Visit t -> GraphSliceT t m FlatExpr
getSuccessVar visit = do
    r <- normalizeVisitM visit
    let Right (Just norm) = r
    getSuccessVarNorm norm

--

compileProofCheckGroup
    :: AsmRefineGraphSliceInput
    -> ProofCheckGroup AsmRefineTag a
    -> SMTProofCheckGroup a
compileProofCheckGroup input group =
    SMTProofCheckGroup setup imps
  where
    (imps, setup) = runWriter $ runAsmRefineGraphSliceT input $ m <* addAccumulatedAssertions
    m = for group $ \check -> SMTProofCheckImp check.meta <$> (interpretCheck >=> convertExpr) check

interpretCheck
    :: (RefineTag t, MonadGraphSliceSendSExpr m)
    => ProofCheck t a
    -> GraphSliceT t m FlatExpr
interpretCheck check = interpretHyp check.hyp >>= interpretHypImps check.hyps

interpretHypImps :: (Tag t, MonadGraphSliceSendSExpr m) => [Hyp t] -> FlatExpr -> GraphSliceT t m FlatExpr
interpretHypImps hyps concl = do
    hyps' <- traverse interpretHyp hyps
    return $ strengthenHyp $ nImpliesE hyps' concl

interpretHyp :: (Tag t, MonadGraphSliceSendSExpr m) => Hyp t -> GraphSliceT t m FlatExpr
interpretHyp = \case
    HypPcImp hyp -> do
        let f = \case
                PcImpHypSideBool v -> return $ fromBoolE v
                PcImpHypSidePc vt -> getPc vt
        impliesE <$> f hyp.lhs <*> f hyp.rhs
    HypEq { ifAt, eq } -> do
        extEnv <- case eq.induct of
            Just induct -> M.insert (Ident "%n") <$> getInductVar induct
            Nothing -> return id
        xPcEnvOpt <- getPcEnv eq.lhs.visit
        yPcEnvOpt <- getPcEnv eq.rhs.visit
        case (xPcEnvOpt, yPcEnvOpt) of
            (Just xPcEnv, Just yPcEnv) -> do
                let eq' = eqHandlingRelWrapper
                        (flattenExpr (extEnv xPcEnv.env) eq.lhs.expr)
                        (flattenExpr (extEnv yPcEnv.env) eq.rhs.expr)
                if ifAt
                    then do
                        xPc <- getPc eq.lhs.visit
                        yPc <- getPc eq.rhs.visit
                        return $ nImpliesE [xPc, yPc] eq'
                    else do
                        return eq'
            _ -> return $ fromBoolE ifAt
