module BV.Core.RepGraph.New.InterpretHyp
    ( interpretHyp
    , interpretHypImps
    ) where

import BV.Core.RepGraph.New

import BV.Core.Logic (strengthenHyp)
import BV.Core.Types
import BV.Core.Types.Extras

import qualified Data.Map as M

interpretHypImps :: (RefineTag t, MonadRepGraphSendSExpr m) => [Hyp t] -> FlatExpr -> RepGraphT t m FlatExpr
interpretHypImps hyps concl = do
    hyps' <- traverse interpretHyp hyps
    return $ strengthenHyp $ nImpliesE hyps' concl

interpretHyp :: (RefineTag t, MonadRepGraphSendSExpr m) => Hyp t -> RepGraphT t m FlatExpr
interpretHyp = \case
    HypPcImp hyp -> do
        let f = \case
                PcImpHypSideBool v -> return $ fromBoolE v
                PcImpHypSidePc vt -> getPcWithTag vt
        impliesE <$> f hyp.lhs <*> f hyp.rhs
    HypEq { ifAt, eq } -> do
        envExt <- case eq.induct of
            Nothing -> do
                return mempty
            Just induct -> do
                var <- getInductVar induct
                return $ M.singleton (Ident "%n") (varFromNameTyE var)
        xPcEnvOpt <- getNodePcEnvWithTag eq.lhs.visit
        yPcEnvOpt <- getNodePcEnvWithTag eq.rhs.visit
        case (xPcEnvOpt, yPcEnvOpt) of
            (Just xPcEnv, Just yPcEnv) -> do
                eq' <- instEqWithEnvs
                    (eq.lhs.expr, envExt <> xPcEnv.env)
                    (eq.rhs.expr, envExt <> yPcEnv.env)
                if ifAt
                    then do
                        xPc <- getPcWithTag eq.lhs.visit
                        yPc <- getPcWithTag eq.rhs.visit
                        return $ nImpliesE [xPc, yPc] eq'
                    else do
                        return eq'
            _ -> do
                return $ fromBoolE ifAt
