{-# LANGUAGE RecordWildCards #-}

module BV.Core.Stages.CompileProofChecks
    ( FunctionSignature (..)
    , FunctionSignatures
    , RepGraphInput (..)
    , compileProofChecks
    ) where

import BV.Core.Logic
import BV.Core.Stages.CompileProofChecks.Grouping
import BV.Core.Stages.CompileProofChecks.RepGraph
import BV.Core.Stages.CompileProofChecks.Solver
import BV.Core.Stages.CompileProofChecks.Structs
import BV.Core.Types
import BV.Core.Types.Extras

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT, mapReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT, mapStateT)
import Control.Monad.Writer (runWriter)
import Data.Map (Map)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics

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

data RepGraphInput
  = RepGraphInput
      { cStructs :: Map Ident Struct
      , functionSigs :: FunctionSignatures
      , pairings :: Pairings
      , rodata :: ROData
      , argRenames :: ArgRenames
      , problem :: Problem
      }
  deriving (Generic)

newtype M m a
  = M { run :: StateT State (ReaderT Env m) a }
  deriving (Functor)
  deriving newtype (Applicative, Monad)

runM :: MonadSolverSend m => RepGraphInput -> M m a -> m a
runM input m = runReaderT (evalStateT m'.run initState) env
  where
    env = initEnv input
    m' = do
        initSolver
        initRepGraph
        m

instance MonadSolverSend m => MonadSolverSend (M m) where
    sendSExprWithPlaceholders = M . sendSExprWithPlaceholders

data Env
  = Env
      { structs :: Ident -> Struct
      , solver :: SolverEnv
      , repGraph :: RepGraphEnv
      }
  deriving (Generic)

data State
  = State
      { solver :: SolverState
      , repGraph :: RepGraphState
      }
  deriving (Generic)

instance Monad m => MonadStructs (M m) where
    askLookupStruct = M $ gview #structs

instance MonadSolverSend m => MonadSolver (M m) where
    liftSolver m = M
        . zoom #solver
        . magnify #solver
        . mapStateT (mapReaderT (return . runIdentity))
        $ m

instance MonadSolverSend m => MonadRepGraph (M m) where
    liftRepGraph m = M
        . zoom #repGraph
        . magnify #repGraph
        . mapStateT (mapReaderT (return . runIdentity))
        $ m

initEnv :: RepGraphInput -> Env
initEnv (RepGraphInput {..}) = Env
    { structs = initStructsEnv rodata problem cStructs
    , solver = initSolverEnv rodata
    , repGraph = initRepGraphEnv functionSigs pairings argRenames problem
    }

initState :: State
initState = State
    { solver = initSolverState
    , repGraph = initRepGraphState
    }

--

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
