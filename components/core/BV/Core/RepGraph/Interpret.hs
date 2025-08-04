module BV.Core.RepGraph.Interpret
    ( interpretHyp
    , interpretHypImps
    ) where

import BV.Core.Logic
import BV.Core.RepGraph.Core
import BV.Core.RepGraph.Solver
import BV.Core.Types
import BV.Core.Types.Extras

interpretHypImps :: (RefineTag t, MonadRepGraph t m) => [Hyp t] -> Expr -> m Expr
interpretHypImps hyps concl = do
    hyps' <- traverse interpretHyp hyps
    return $ strengthenHyp $ nImpliesE hyps' concl

interpretHyp :: (RefineTag t, MonadRepGraph t m) => Hyp t -> m Expr
interpretHyp = \case
    HypPcImp hyp -> do
        let f = \case
                PcImpHypSideBool v -> return $ fromBoolE v
                PcImpHypSidePc vt -> getPc vt
        impliesE <$> f hyp.lhs <*> f hyp.rhs
    HypEq { ifAt, eq } -> do
        (x, y) <- case eq.induct of
            Nothing -> return (eq.lhs.expr, eq.rhs.expr)
            Just induct -> do
                v <- getInductVar induct
                let x = substInduct eq.lhs.expr v
                let y = substInduct eq.rhs.expr v
                return (x, y)
        xPcEnvOpt <- getNodePcEnv eq.lhs.visit
        yPcEnvOpt <- getNodePcEnv eq.rhs.visit
        case (xPcEnvOpt, yPcEnvOpt) of
            (Just xPcEnv, Just yPcEnv) -> do
                eq' <- instEqWithEnvs (x, xPcEnv.env) (y, yPcEnv.env)
                if ifAt
                    then do
                        xPc <- getPc eq.lhs.visit
                        yPc <- getPc eq.rhs.visit
                        return $ nImpliesE [xPc, yPc] eq'
                    else do
                        return eq'
            _ -> do
                return $ fromBoolE ifAt
