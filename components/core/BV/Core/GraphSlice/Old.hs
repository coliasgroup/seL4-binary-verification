module BV.Core.GraphSlice.Old
    ( AsmRefineGraphSliceInput (..)
    , ExprEnv
    , FlatExpr
    , FunCallInfo (..)
    , GraphSliceExport (..)
    , GraphSliceHooks
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
    , compileProofCheckGroup
    , convertExpr
    , defaultGraphSliceHooks
    , flattenExpr
    , getExport
    , getFunCallInfo
    , getFunCallVisitsCompat
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
    , withAsmStackSplitting
    , withConstRetAssumptions
    , withFast
    ) where

import BV.Core.GraphSlice.Old.Core
import BV.Core.GraphSlice.Old.Solver

import BV.Core.GraphSlice.New (AsmRefineGraphSliceInput (..), FlatExpr,
                               GraphSliceInput (..))
import BV.Core.GraphSlice.New.Common
import BV.Core.GraphSlice.New.Flatten (GraphSliceExport (..))
import BV.Core.GraphSlice.New.PcEnv
import BV.Core.GraphSlice.New.Tagged

import BV.Core.Logic (strengthenHyp)
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Utils (ensure)

import Control.Monad.Writer (runWriter)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Traversable (for)

runGraphSliceT
    :: (Tag t, MonadGraphSliceSendSExpr m)
    => GraphSliceHooks t
    -> GraphSliceInput t
    -> GraphSliceT t m a
    -> m a
runGraphSliceT hooks input =
      runGraphSliceSolverTStep structs input.rodata
    . runGraphSliceTStep input.problem hooks
  where
    -- TODO scope structs with tag and use `M.unionsWith undefined`
    structs = (M.!) $ M.unionsWith ensureEq $
        rodataStructsOf input.rodata : toList input.structs
    ensureEq x y = ensure (x == y) x

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

compileProofCheckGroup
    :: AsmRefineGraphSliceInput
    -> ProofCheckGroup AsmRefineTag a
    -> SMTProofCheckGroup a
compileProofCheckGroup input group =
    SMTProofCheckGroup setup imps
  where
    (imps, setup) = runWriter (runAsmRefineGraphSliceT input m)
    m = interpretGroup group <* addAccumulatedAssertions

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

--

getExport :: Monad m => GraphSliceT t m (GraphSliceExport t)
getExport = undefined
