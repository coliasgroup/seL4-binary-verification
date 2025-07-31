module BV.Core.RepGraph.Interpret
    ( interpretHyp
    , interpretHypImp
    ) where

import BV.Core.Logic
import BV.Core.RepGraph.Core
import BV.Core.RepGraph.Solver
import BV.Core.Stages.GroupProofChecks
import BV.Core.Types
import BV.Core.Types.Extras

import Control.Monad.Writer (runWriter)
import Data.Traversable (for)

interpretHypImp :: (RefineTag t, MonadRepGraph t m) => [Hyp t] -> Expr -> m Expr
interpretHypImp hyps concl = do
    hyps' <- traverse interpretHyp hyps
    return $ strengthenHyp $ nImpliesE hyps' concl

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
