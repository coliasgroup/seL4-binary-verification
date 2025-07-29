{-# LANGUAGE RecordWildCards #-}

module BV.Core.Stages.CompileProofChecks
    ( FunctionSignature (..)
    , FunctionSignatures
    , RepGraphBaseInput (..)
    , compileProofChecks
    ) where

import BV.Core.Logic
import BV.Core.Stages.CompileProofChecks.RepGraph
import BV.Core.Stages.CompileProofChecks.RepGraph.Solver
import BV.Core.Stages.GroupProofChecks
import BV.Core.Types
import BV.Core.Types.Extras

import Control.Monad.Writer (runWriter)
import Data.Traversable (for)

compileProofChecks
    :: RepGraphBaseInput AsmRefineTag
    -> Pairings'
    -> ArgRenames AsmRefineTag
    -> [ProofCheck AsmRefineTag a]
    -> [SMTProofCheckGroup a]
compileProofChecks repGraphInput pairings argRenames checks =
    map
        (compileProofCheckGroup repGraphInput pairings argRenames)
        (proofCheckGroups checks)

compileProofCheckGroup
    :: RepGraphBaseInput AsmRefineTag
    -> Pairings'
    -> ArgRenames AsmRefineTag
    -> ProofCheckGroup AsmRefineTag a
    -> SMTProofCheckGroup a
compileProofCheckGroup repGraphInput pairings argRenames group =
    SMTProofCheckGroup setup imps
  where
    (imps, setup) = runWriter (runRepGraphBase repGraphInput (runWithAddFunc pairings (runWithAsmStackRep argRenames m)))
    m = interpretGroup group <* finalizeSolver

interpretGroup :: (RefineTag t, MonadRepGraph t m) => ProofCheckGroup t a -> m [SMTProofCheckImp a]
interpretGroup group = do
    hyps <- for group $ \check -> do
        concl <- interpretHyp check.hyp
        hyps <- mapM interpretHyp check.hyps
        return (check, strengthenHyp (nImpliesE hyps concl))
    for hyps $ \(check, term) -> do
        sexpr <- withoutEnv $ convertExprNoSplit term
        return $ SMTProofCheckImp check.meta sexpr

interpretHyp :: (RefineTag t, MonadRepGraph t m) => Hyp t -> m Expr
interpretHyp = \case
    HypPcImp hyp -> do
        let f = \case
                PcImpHypSideBool v -> return $ fromBoolE v
                PcImpHypSidePc vt -> getPc vt.value (Just vt.tag)
        impliesE <$> f hyp.lhs <*> f hyp.rhs
    HypEq { ifAt, eq } -> do
        (x, y) <- case eq.induct of
            Nothing -> return (eq.lhs.expr, eq.rhs.expr)
            Just induct -> do
                v <- getInductVar induct
                let x = substInduct eq.lhs.expr v
                let y = substInduct eq.rhs.expr v
                return (x, y)
        xPcEnvOpt <- getNodePcEnv eq.lhs.visit.value (Just eq.lhs.visit.tag)
        yPcEnvOpt <- getNodePcEnv eq.rhs.visit.value (Just eq.rhs.visit.tag)
        case (xPcEnvOpt, yPcEnvOpt) of
            (Just xPcEnv, Just yPcEnv) -> do
                eq' <- instEqWithEnvs (x, xPcEnv.env) (y, yPcEnv.env)
                if ifAt
                    then do
                        xPc <- getPc eq.lhs.visit.value (Just eq.lhs.visit.tag)
                        yPc <- getPc eq.rhs.visit.value (Just eq.rhs.visit.tag)
                        return $ nImpliesE [xPc, yPc] eq'
                    else do
                        return eq'
            _ -> do
                return $ fromBoolE ifAt
