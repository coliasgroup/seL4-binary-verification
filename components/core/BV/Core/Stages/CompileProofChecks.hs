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
    -> LookupFunctionSignature AsmRefineTag
    -> Pairings'
    -> [ProofCheck AsmRefineTag a]
    -> [(ProofCheckGroupCheckIndices, SMTProofCheckGroup a)]
compileProofChecks repGraphInput lookupSig pairings checks =
    over (traversed % _2)
        (compileProofCheckGroup repGraphInput lookupSig pairings . pruneGroup)
        (proofCheckGroups checks)
  where
    pruneCheck = pruneProofCheck (analyzeProblem repGraphInput.problem)
    pruneGroup = map pruneCheck

compileProofCheckGroup
    :: RepGraphBaseInput AsmRefineTag
    -> LookupFunctionSignature AsmRefineTag
    -> Pairings'
    -> ProofCheckGroup AsmRefineTag a
    -> SMTProofCheckGroup a
compileProofCheckGroup repGraphInput lookupSig pairings group =
    SMTProofCheckGroup setup imps
  where
    (imps, setup) =
        runWriter
            (runRepGraphBase
                repGraphInput
                (runWithFunAsserts lookupSig pairings (runWithAsmStackRep argRenames m)))
    argRenames =
        problemArgRenames repGraphInput.problem $
            lookupSig <$>
                withTags (pairingIdOfProblem repGraphInput.problem)
    m = interpretGroup group <* addPValidDomAssertions

interpretGroup :: (RefineTag t, MonadRepGraph t m) => ProofCheckGroup t a -> m [SMTProofCheckImp a]
interpretGroup group = do
    hyps <- for group $ \check -> do
        concl <- interpretHyp check.hyp
        expr <- interpretHypImps check.hyps concl
        return (check, expr)
    for hyps $ \(check, expr) -> do
        sexpr <- convertSolverExpr expr
        return $ SMTProofCheckImp check.meta sexpr
