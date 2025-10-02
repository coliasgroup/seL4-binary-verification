module BV.Core.GraphSlice.Old
    ( AsmRefineGraphSliceInput (..)
    , ExprEnv
    , FlatExpr
    , FunCallInfo (..)
    , GraphSliceHooks (preEmitCallNodeHook)
    , GraphSliceInput (..)
    , GraphSliceT
    , GraphSliceTaggedT
    , MonadGraphSliceSendSExpr (..)
    , PcEnv (..)
    , addAccumulatedAssertions
    , askContVisit
    , askLoopData
    , askNodeGraph
    , askProblem
    , askTag
    , askWithTag
    , asmRefineGraphSliceHooks
    , assertExpr
    , convertExpr
    , defaultGraphSliceHooks
    , flattenExpr
    , getFunCallInfo
    , getInductVar
    , getNodePcEnv
    , getNodePcEnvWithTag
    , getPc
    , getPcWithTag
    , instEqWithEnvs
    , interpretGroup
    , interpretHyp
    , interpretHypImps
    , liftUntagged
    , runAsmRefineGraphSliceT
    , runGraphSliceT
    , runGraphSliceTStep
    , runTagged
    , tryGetNodePcEnv
    ) where

import BV.Core.GraphSlice.Old.Core
import BV.Core.GraphSlice.Old.Solver

import BV.Core.GraphSlice.New (AsmRefineGraphSliceInput (..), FlatExpr,
                               GraphSliceInput (..))
import BV.Core.GraphSlice.New.Common
import BV.Core.GraphSlice.New.Flatten.PcEnv
import BV.Core.GraphSlice.New.Flatten.Tagged

import BV.Core.Logic (strengthenHyp)
import BV.Core.Types
import BV.Core.Types.Extras

import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Traversable (for)

runGraphSliceT
    :: (Tag t, MonadGraphSliceSendSExpr m)
    => GraphSliceHooks t m
    -> GraphSliceInput t
    -> GraphSliceT t m a
    -> m a
runGraphSliceT hooks input =
      runGraphSliceSolverTStep structs input.rodata
    . runGraphSliceTStep input.problem hooks
  where
    structs = (M.!) $ M.unionsWith undefined $
        rodataStructsOf input.rodata : toList input.structs

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

assertExpr :: (Tag t, MonadGraphSliceSendSExpr m) => FlatExpr -> GraphSliceT t m ()
assertExpr = liftInner . assertFact

convertExpr :: (Tag t, MonadGraphSliceSendSExpr m) => FlatExpr -> GraphSliceT t m SExprWithPlaceholders
convertExpr = liftInner . convertExprNotSplit

getPcWithTag :: (Tag t, MonadGraphSliceSendSExpr m) => WithTag t Visit -> GraphSliceT t m FlatExpr
getPcWithTag (WithTag tag visit) = fmap castExpr $ runTagged tag $ getPc visit

getNodePcEnvWithTag :: (Tag t, MonadGraphSliceSendSExpr m) => WithTag t Visit -> GraphSliceT t m (Maybe PcEnv)
getNodePcEnvWithTag (WithTag tag visit) = runTagged tag $ getNodePcEnv visit

addAccumulatedAssertions :: (Tag t, MonadGraphSliceSendSExpr m) => GraphSliceT t m ()
addAccumulatedAssertions = do
    liftInner $ sendAccumulatedSolverAssertions

--

interpretGroup
    :: (RefineTag t, MonadGraphSliceSendSExpr m)
    => ProofCheckGroup t a
    -> GraphSliceT t m [SMTProofCheckImp a]
interpretGroup group = do
    hyps <- for group $ \check -> do
        concl <- interpretHyp check.hyp
        expr <- interpretHypImps check.hyps concl
        return (check, expr)
    for hyps $ \(check, expr) -> do
        sexpr <- convertExpr expr
        return $ SMTProofCheckImp check.meta sexpr

interpretHypImps :: (RefineTag t, MonadGraphSliceSendSExpr m) => [Hyp t] -> FlatExpr -> GraphSliceT t m FlatExpr
interpretHypImps hyps concl = do
    hyps' <- traverse interpretHyp hyps
    return $ strengthenHyp $ nImpliesE hyps' concl

interpretHyp :: (RefineTag t, MonadGraphSliceSendSExpr m) => Hyp t -> GraphSliceT t m FlatExpr
interpretHyp = \case
    HypPcImp hyp -> do
        let f = \case
                PcImpHypSideBool v -> return $ fromBoolE v
                PcImpHypSidePc vt -> runWithTag getPc vt
        impliesE <$> f hyp.lhs <*> f hyp.rhs
    HypEq { ifAt, eq } -> do
        extEnv <- case eq.induct of
            Just induct -> M.insert (Ident "%n") <$> getInductVar induct
            Nothing -> return id
        xPcEnvOpt <- runWithTag getNodePcEnv eq.lhs.visit
        yPcEnvOpt <- runWithTag getNodePcEnv eq.rhs.visit
        case (xPcEnvOpt, yPcEnvOpt) of
            (Just xPcEnv, Just yPcEnv) -> do
                eq' <- instEqWithEnvs
                        (eq.lhs.expr, extEnv xPcEnv.env)
                        (eq.rhs.expr, extEnv yPcEnv.env)
                if ifAt
                    then do
                        xPc <- runWithTag getPc eq.lhs.visit
                        yPc <- runWithTag getPc eq.rhs.visit
                        return $ nImpliesE [xPc, yPc] eq'
                    else do
                        return eq'
            _ -> do
                return $ fromBoolE ifAt
