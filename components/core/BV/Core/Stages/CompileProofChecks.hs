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
    ensure (all visitNodesAreDistinct group) $
    SMTProofCheckGroup setup imps
  where
    (imps, setup) =
        runWriter
            (runRepGraphBase
                repGraphInput
                (runWithAddFunc lookupSig pairings (runWithAsmStackRep argRenames m)))
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
        sexpr <- withoutEnv $ convertExprNotSplit expr
        return $ SMTProofCheckImp check.meta sexpr

visitNodesAreDistinct :: RefineTag t => ProofCheck t a -> Bool
visitNodesAreDistinct check =
    applyWhen (not ok) f ok
  where
    f = trace $ intercalate "\n" (map show visits)
    visits = check ^.. checkVisits % #value
    visitOk :: Visit -> Bool
    visitOk visit = S.size restrs == S.size nodes
      where
        restrs = S.fromList visit.restrs
        nodes = S.map (view #nodeAddr) restrs
    ok = all visitOk visits
