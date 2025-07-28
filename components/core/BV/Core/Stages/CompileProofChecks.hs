{-# LANGUAGE RecordWildCards #-}

module BV.Core.Stages.CompileProofChecks
    ( FunctionSignature (..)
    , FunctionSignatures
    , RepGraphInput (..)
    , compileProofChecks
    ) where

import BV.Core.Logic
import BV.Core.Stages.CompileProofChecks.RepGraph.Concrete
import BV.Core.Stages.CompileProofChecks.Grouping
import BV.Core.Stages.CompileProofChecks.RepGraph
import BV.Core.Stages.CompileProofChecks.Solver
import BV.Core.Types
import BV.Core.Types.Extras

import Control.Monad.Writer (runWriter)
import Data.Traversable (for)

compileProofChecks
    :: RepGraphInput
    -> [ProofCheck a]
    -> [SMTProofCheckGroup a]
compileProofChecks input checks =
    map
        (compileProofCheckGroup input)
        (proofCheckGroups checks)

compileProofCheckGroup
    :: RepGraphInput
    -> ProofCheckGroup a
    -> SMTProofCheckGroup a
compileProofCheckGroup input group =
    SMTProofCheckGroup setup imps
  where
    (imps, setup) = runWriter (runM input m)
    m = interpretGroup group <* finalizeSolver

interpretGroup :: MonadSolverSend m => ProofCheckGroup a -> M m [SMTProofCheckImp a]
interpretGroup group = do
    hyps <- for group $ \check -> do
        concl <- interpretHyp check.hyp
        hyps <- mapM interpretHyp check.hyps
        return (check, strengthenHyp (nImpliesE hyps concl))
    for hyps $ \(check, term) -> do
        sexpr <- withoutEnv $ convertExprNoSplit term
        return $ SMTProofCheckImp check.meta sexpr

interpretHyp :: MonadSolverSend m => Hyp -> M m Expr
interpretHyp = \case
    HypPcImp hyp -> do
        let f = \case
                PcImpHypSideBool v -> return $ fromBoolE v
                PcImpHypSidePc vt -> getPc vt.visit (Just vt.tag)
        impliesE <$> f hyp.lhs <*> f hyp.rhs
    HypEq { ifAt, eq } -> do
        (x, y) <- case eq.induct of
            Nothing -> return (eq.lhs.expr, eq.rhs.expr)
            Just induct -> do
                v <- getInductVar induct
                let x = substInduct eq.lhs.expr v
                let y = substInduct eq.rhs.expr v
                return (x, y)
        xPcEnv <- getNodePcEnv eq.lhs.visit.visit (Just eq.lhs.visit.tag)
        yPcEnv <- getNodePcEnv eq.rhs.visit.visit (Just eq.rhs.visit.tag)
        case (xPcEnv, yPcEnv) of
            (Just (_, xEnv), Just (_, yEnv)) -> do
                eq' <- instEqWithEnvs (x, xEnv) (y, yEnv)
                if ifAt
                    then do
                        xPc <- getPc eq.lhs.visit.visit (Just eq.lhs.visit.tag)
                        yPc <- getPc eq.rhs.visit.visit (Just eq.rhs.visit.tag)
                        return $ nImpliesE [xPc, yPc] eq'
                    else do
                        return eq'
            _ -> do
                return $ fromBoolE ifAt
