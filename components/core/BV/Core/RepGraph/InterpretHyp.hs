module BV.Core.RepGraph.InterpretHyp
    ( interpretHyp
    , interpretHypImps
    ) where

import BV.Core.Logic
import BV.Core.RepGraph.Core
import BV.Core.RepGraph.Solver
import BV.Core.Types
import BV.Core.Types.Extras

interpretHypImps :: (RefineTag t, MonadRepGraph t m) => [Hyp t] -> SolverExpr -> m SolverExpr
interpretHypImps hyps concl = do
    hyps' <- traverse interpretHyp hyps
    return $ strengthenHyp $ nImpliesE hyps' concl

interpretHyp :: (RefineTag t, MonadRepGraph t m) => Hyp t -> m SolverExpr
interpretHyp = \case
    HypPcImp hyp -> do
        let f = \case
                PcImpHypSideBool v -> return $ fromBoolE v
                PcImpHypSidePc vt -> getPcWithTag vt
        impliesE <$> f hyp.lhs <*> f hyp.rhs
    HypEq { ifAt, eq } -> do
        (x, y) <- case eq.induct of
            Nothing -> return (eq.lhs.expr, eq.rhs.expr)
            Just induct -> do
                v <- getInductVar induct
                let x = substInduct eq.lhs.expr v
                let y = substInduct eq.rhs.expr v
                return (x, y)
        xPcEnvOpt <- getNodePcEnvWithTag eq.lhs.visit
        yPcEnvOpt <- getNodePcEnvWithTag eq.rhs.visit
        case (xPcEnvOpt, yPcEnvOpt) of
            (Just xPcEnv, Just yPcEnv) -> do
                eq' <- instEqWithEnvs (x, xPcEnv.env) (y, yPcEnv.env)
                if ifAt
                    then do
                        xPc <- getPcWithTag eq.lhs.visit
                        yPc <- getPcWithTag eq.rhs.visit
                        return $ nImpliesE [xPc, yPc] eq'
                    else do
                        return eq'
            _ -> do
                return $ fromBoolE ifAt
