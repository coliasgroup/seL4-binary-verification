module BV.Core.Stages.CompileProofChecks
    ( compileProofChecks
    ) where

import BV.Core.GraphSlice
import BV.Core.Stages.EnumerateProofChecks (pruneProofCheck)
import BV.Core.Stages.GroupProofChecks
import BV.Core.Types
import BV.Core.Types.Extras

import Control.Monad.Writer (runWriter)
import Data.Traversable (for)
import Optics

compileProofChecks
    :: AsmRefineGraphSliceInput
    -> [ProofCheck AsmRefineTag a]
    -> [(ProofCheckGroupCheckIndices, SMTProofCheckGroup a)]
compileProofChecks input checks =
    over (traversed % _2)
        (compileProofCheckGroup input . pruneGroup)
        (proofCheckGroups checks)
  where
    pruneCheck = pruneProofCheck (analyzeProblem input.repGraphInput.problem)
    pruneGroup = map pruneCheck

compileProofCheckGroup
    :: AsmRefineGraphSliceInput
    -> ProofCheckGroup AsmRefineTag a
    -> SMTProofCheckGroup a
compileProofCheckGroup input group =
    SMTProofCheckGroup setup imps
  where
    (imps, setup) = runWriter (runAsmRefineGraphSliceT input m)
    m = interpretGroup group <* addAccumulatedAssertions

interpretGroup :: (RefineTag t, MonadGraphSliceSendSExpr m) => ProofCheckGroup t a -> GraphSliceT t m [SMTProofCheckImp a]
interpretGroup group = do
    hyps <- for group $ \check -> do
        concl <- interpretHyp check.hyp
        expr <- interpretHypImps check.hyps concl
        return (check, expr)
    for hyps $ \(check, expr) -> do
        sexpr <- convertExpr expr
        return $ SMTProofCheckImp check.meta sexpr
