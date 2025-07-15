module BV.Core.Stages.CompileProofChecks
    ( FunctionSignature (..)
    , FunctionSignatures
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
    :: Map Ident Struct
    -> FunctionSignatures
    -> Pairings
    -> ROData
    -> ArgRenames
    -> Problem
    -> [ProofCheck a]
    -> [SMTProofCheckGroup a]
compileProofChecks cStructs functionSigs pairings rodata argRenames problem checks =
    map
        (compileProofCheckGroup cStructs functionSigs pairings rodata argRenames problem)
        (proofCheckGroups checks)

compileProofCheckGroup
    :: Map Ident Struct
    -> FunctionSignatures
    -> Pairings
    -> ROData
    -> ArgRenames
    -> Problem
    -> ProofCheckGroup a
    -> SMTProofCheckGroup a
compileProofCheckGroup cStructs functionSigs pairings rodata argRenames problem group =
    SMTProofCheckGroup setup imps
  where
    (imps, setup) = runWriter (runReaderT (evalStateT m.run initState) env)
    env = initEnv rodata cStructs functionSigs pairings argRenames problem
    m = do
        initSolver
        initRepGraph
        imps' <- interpretGroup group
        finalizeSolver
        return imps'

newtype M m a
  = M { run :: StateT State (ReaderT Env m) a }
  deriving (Functor)
  deriving newtype (Applicative, Monad)

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

initEnv :: ROData -> Map Ident Struct -> FunctionSignatures -> Pairings -> ArgRenames -> Problem -> Env
initEnv rodata cStructs functionSigs pairings argRenames problem = Env
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
