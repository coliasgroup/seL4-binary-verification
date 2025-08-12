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
import BV.Utils (ensure)

import Control.Monad.Writer (runWriter)
import qualified Data.Set as S
import Data.Traversable (for)
import Optics
import Data.Function (applyWhen)
import Debug.Trace (trace)
import Data.List (intercalate)

compileProofChecks
    :: RepGraphBaseInput AsmRefineTag
    -> FunctionSignatures AsmRefineTag
    -> Pairings'
    -> ArgRenames AsmRefineTag
    -> [ProofCheck AsmRefineTag a]
    -> [(ProofCheckGroupCheckIndices, SMTProofCheckGroup a)]
compileProofChecks repGraphInput functionSigs pairings argRenames unprunedChecks =
    ensure (all visitNodesAreDistinct prunedChecks) $
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

visitNodesAreDistinct :: RefineTag t => ProofCheck t a -> Bool
visitNodesAreDistinct check =
    applyWhen (not ok) f ok
  where
    f = trace $ intercalate "\n" (map show (S.toList visits))
    visits = S.fromList $ check ^.. checkVisits
    nodes = S.map (fmap (view #nodeId)) visits
    ok = S.size visits == S.size nodes
