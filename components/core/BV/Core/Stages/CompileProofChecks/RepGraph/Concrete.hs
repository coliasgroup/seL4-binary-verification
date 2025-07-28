{-# LANGUAGE RecordWildCards #-}

module BV.Core.Stages.CompileProofChecks.RepGraph.Concrete
    ( M
    , RepGraphInput (..)
    , runM
    ) where

import BV.Core.Logic
import BV.Core.Stages.CompileProofChecks.RepGraph
import BV.Core.Stages.CompileProofChecks.Solver
import BV.Core.Stages.CompileProofChecks.Structs
import BV.Core.Types

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT, mapReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT, mapStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Map (Map)
import GHC.Generics (Generic)
import Optics

data RepGraphInput
  = RepGraphInput
      { cStructs :: Map Ident Struct
      , rodata :: ROData
      , problem :: Problem
      , functionSigs :: FunctionSignatures
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

instance MonadTrans M where
    lift = M . lift . lift

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

    runProblemVarRepHook _ _ _ _ = return Nothing
    runPostEmitNodeHook _ = return ()
    runPreEmitCallNodeHook _ _ _ = return ()
    runPostEmitCallNodeHook _ _ _ _ = return ()

initEnv :: RepGraphInput -> Env
initEnv (RepGraphInput {..}) = Env
    { structs = initStructsEnv rodata problem cStructs
    , solver = initSolverEnv rodata
    , repGraph = initRepGraphEnv problem functionSigs
    }

initState :: State
initState = State
    { solver = initSolverState
    , repGraph = initRepGraphState
    }
