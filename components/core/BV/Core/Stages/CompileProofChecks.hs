module BV.Core.Stages.CompileProofChecks
    ( FunctionSignature (..)
    , RepGraphBaseInput (..)
    , compileProofChecks
    ) where

import BV.Core.RepGraph
import BV.Core.Stages.EnumerateProofChecks (pruneProofCheck)
import BV.Core.Stages.GroupProofChecks
import BV.Core.Types
import BV.Core.Types.Extras

import Control.Monad.Writer (runWriter)
import Data.Traversable (for)
import Optics

compileProofChecks
    :: RepGraphBaseInput AsmRefineTag
    -> FunctionSignatures AsmRefineTag
    -> Pairings'
    -> ArgRenames AsmRefineTag
    -> [ProofCheck AsmRefineTag a]
    -> [(ProofCheckGroupCheckIndices, SMTProofCheckGroup a)]
compileProofChecks repGraphInput functionSigs pairings argRenames unprunedChecks =
    over (traversed % _2)
        (compileProofCheckGroup repGraphInput functionSigs pairings argRenames)
        (proofCheckGroups prunedChecks)
  where
    prunedChecks = map (pruneProofCheck (analyzeProblem repGraphInput.problem)) unprunedChecks

compileProofCheckGroup
    :: RepGraphBaseInput AsmRefineTag
    -> FunctionSignatures AsmRefineTag
    -> Pairings'
    -> ArgRenames AsmRefineTag
    -> ProofCheckGroup AsmRefineTag a
    -> SMTProofCheckGroup a
compileProofCheckGroup repGraphInput functionSigs pairings argRenames group =
    SMTProofCheckGroup setup imps
  where
    (imps, setup) = runWriter (runRepGraphBase repGraphInput (runWithAddFunc functionSigs pairings (runWithAsmStackRep argRenames m)))
    m = interpretGroup group <* addPValidDomAssertions

interpretGroup :: (RefineTag t, MonadRepGraph t m) => ProofCheckGroup t a -> m [SMTProofCheckImp a]
interpretGroup group = do
    hyps <- for group $ \check -> do
        concl <- interpretHyp check.hyp
        expr <- interpretHypImps check.hyps concl
        return (check, expr)
    for hyps $ \(check, expr) -> do
        sexpr <- withoutEnv $ convertExprNoSplit expr
        return $ SMTProofCheckImp check.meta sexpr
