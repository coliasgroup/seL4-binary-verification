module BV.Core.Experimental.RepGraph.InterpretHyp
    ( interpretHyp
    , interpretHypImps
    ) where

import BV.Core.Experimental.RepGraph.Core
import BV.Core.Experimental.RepGraph.Types

import BV.Core.Logic
import BV.Core.Types
import BV.Core.Types.Extras

import qualified Data.Map as M

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
        envExt <- case eq.induct of
            Nothing -> return mempty
            Just induct -> do
                v <- getInductVar induct
                return $ M.singleton (Ident "%n") (varFromNameTyE v)
        xPcEnvOpt <- getNodePcEnvWithTag eq.lhs.visit
        yPcEnvOpt <- getNodePcEnvWithTag eq.rhs.visit
        case (xPcEnvOpt, yPcEnvOpt) of
            (Just xPcEnv, Just yPcEnv) -> do
                eq' <- instEqWithEnvs (eq.lhs.expr, xPcEnv.env <> envExt) (eq.rhs.expr, yPcEnv.env <> envExt)
                if ifAt
                    then do
                        xPc <- getPcWithTag eq.lhs.visit
                        yPc <- getPcWithTag eq.rhs.visit
                        return $ nImpliesE [xPc, yPc] eq'
                    else do
                        return eq'
            _ -> do
                return $ fromBoolE ifAt
