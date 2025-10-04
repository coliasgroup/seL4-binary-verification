module BV.Core.GraphSlice.New
    ( AsmRefineGraphSliceInput (..)
    , ExprEnv
    , FlatExpr
    , FunCallInfo (..)
    , GraphSliceExport (..)
    , GraphSliceHooks (preEmitCallNode)
    , GraphSliceInput (..)
    , GraphSlicePreEmitCallNodeHook (..)
    , GraphSlicePreEmitCallNodeHookFn (..)
    , GraphSliceT
    , GraphSliceTaggedT
    , GraphSliceWithPreEmitCallHookFnT
    , GraphSliceWithPreEmitCallHookT
    , MonadGraphSliceSendSExpr (..)
    , PcEnv (..)
    , addAccumulatedAssertions
    , askContVisit
    , askExport
    , askLoopData
    , askNodeGraph
    , askProblem
    , askTag
    , askWithTag
    , asmRefineGraphSliceHooks
    , assertExpr
    , compileProofCheckGroup
    , convertExpr
    , defaultGraphSliceHooks
    , flattenExpr
    , getFunCallInfo
    , getInductVar
    , getNodePcEnv
    , getNodePcEnvWithTag
    , getPc
    , getPcWithTag
    , interpretCheck
    , interpretHyp
    , interpretHypImps
    , liftUntagged
    , mapGraphSliceT
    , mapGraphSliceTaggedT
    , runAsmRefineGraphSliceT
    , runGraphSliceT
    , runTagged
    , tryGetNodePcEnv
    ) where

import BV.Core.GraphSlice.New.Common
import BV.Core.GraphSlice.New.Flat
import BV.Core.GraphSlice.New.Flatten
import BV.Core.GraphSlice.New.PcEnv
import BV.Core.GraphSlice.New.SendFlatExprCommand
import BV.Core.GraphSlice.New.SendSolverExprCommand
import BV.Core.GraphSlice.New.Tagged

import BV.Core.Logic (eqHandlingRelWrapper, strengthenHyp)
import BV.Core.Types
import BV.Core.Types.Extras

import Control.Monad ((>=>))
import Control.Monad.Writer (runWriter)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Traversable (for)
import GHC.Generics (Generic)

data GraphSliceInput t
  = GraphSliceInput
      { structs :: ByTag t (M.Map Ident Struct)
      , rodata :: ROData
      , problem :: Problem t
      }
  deriving (Generic)

runGraphSliceT
    :: (Tag t, MonadGraphSliceSendSExpr m)
    => GraphSliceHooks h t
    -> GraphSliceInput t
    -> GraphSliceWithPreEmitCallHookT h t m a
    -> m a
runGraphSliceT hooks input =
      runGraphSliceSendSolverExprCommandTStep input.rodata
    . runGraphSliceSendFlatExprCommandTStep structs (rodataPtrsFromROData input.rodata)
    . runGraphSliceFlatTStep
    . runGraphSliceTStep input.problem hooks
  where
    structs = (M.!) $ M.unionsWith undefined $
        rodataStructsOf input.rodata : toList input.structs

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
    argRenames =
        problemArgRenames input.repGraphInput.problem $
            input.lookupSig <$>
                withTags (pairingIdOfProblem input.repGraphInput.problem)
    hooks = asmRefineGraphSliceHooks input.lookupSig input.pairings argRenames

--

mapGraphSliceT
    :: (forall a. m a -> n a)
    -> GraphSliceT t m b
    -> GraphSliceT t n b
mapGraphSliceT = mapInnermost

assertExpr :: (Tag t, MonadGraphSliceSendSExpr m) => FlatExpr -> GraphSliceT t m ()
assertExpr = liftInner . assertFlatExpr

convertExpr :: (Tag t, MonadGraphSliceSendSExpr m) => FlatExpr -> GraphSliceWithPreEmitCallHookT h t m SExprWithPlaceholders
convertExpr =
    liftInner . liftInner . convertFlatExpr
        >=> liftInner . liftInner . liftInner . convertSolverExpr

getPcWithTag :: (Tag t, MonadGraphSliceSendSExpr m) => WithTag t Visit -> GraphSliceT t m FlatExpr
getPcWithTag = runWithTag getPc

getNodePcEnvWithTag :: (Tag t, MonadGraphSliceSendSExpr m) => WithTag t Visit -> GraphSliceT t m (Maybe PcEnv)
getNodePcEnvWithTag = runWithTag getNodePcEnv

addAccumulatedAssertions :: (Tag t, MonadGraphSliceSendSExpr m) => GraphSliceT t m ()
addAccumulatedAssertions = do
    liftInner $ liftInner $ sendAccumulatedAssertionsInner

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

interpretHypImps :: (RefineTag t, MonadGraphSliceSendSExpr m) => [Hyp t] -> FlatExpr -> GraphSliceT t m FlatExpr
interpretHypImps hyps concl = do
    hyps' <- traverse interpretHyp hyps
    return $ strengthenHyp $ nImpliesE hyps' concl

interpretHyp :: (RefineTag t, MonadGraphSliceSendSExpr m) => Hyp t -> GraphSliceT t m FlatExpr
interpretHyp = \case
    HypPcImp hyp -> do
        let f = \case
                PcImpHypSideBool v -> return $ fromBoolE v
                PcImpHypSidePc vt -> getPcWithTag vt
        impliesE <$> f hyp.lhs <*> f hyp.rhs
    HypEq { ifAt, eq } -> do
        extEnv <- case eq.induct of
            Just induct -> M.insert (Ident "%n") <$> getInductVar induct
            Nothing -> return id
        xPcEnvOpt <- getNodePcEnvWithTag eq.lhs.visit
        yPcEnvOpt <- getNodePcEnvWithTag eq.rhs.visit
        case (xPcEnvOpt, yPcEnvOpt) of
            (Just xPcEnv, Just yPcEnv) -> do
                let eq' = eqHandlingRelWrapper
                        (flattenExpr (extEnv xPcEnv.env) eq.lhs.expr)
                        (flattenExpr (extEnv yPcEnv.env) eq.rhs.expr)
                if ifAt
                    then do
                        xPc <- getPcWithTag eq.lhs.visit
                        yPc <- getPcWithTag eq.rhs.visit
                        return $ nImpliesE [xPc, yPc] eq'
                    else do
                        return eq'
            _ -> return $ fromBoolE ifAt
