module BV.Core.Stages.CompileProofChecks
    ( compileProofChecks
    ) where

import BV.Core.GraphSlice
import BV.Core.Stages.GroupProofChecks
import BV.Core.Types
import BV.Core.Types.Extras

import Control.Monad.Writer (runWriter)
import Optics

compileProofChecks
    :: AsmRefineGraphSliceInput
    -> [ProofCheck AsmRefineTag a]
    -> [(ProofCheckGroupCheckIndices, SMTProofCheckGroup a)]
compileProofChecks input checks =
    over (traversed % _2)
        (compileProofCheckGroup input)
        (prunedProofCheckGroups (analyzeProblem input.repGraphInput.problem) checks)

compileProofCheckGroup
    :: AsmRefineGraphSliceInput
    -> ProofCheckGroup AsmRefineTag a
    -> SMTProofCheckGroup a
compileProofCheckGroup input group =
    SMTProofCheckGroup setup imps
  where
    (imps, setup) = runWriter (runAsmRefineGraphSliceT input m)
    m = interpretGroup group <* addAccumulatedAssertions
