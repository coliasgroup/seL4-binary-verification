{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Environment where

import Control.Monad.Logger (MonadLogger)
import GHC.Generics (Generic)

import BV.SMTLIB2.Types

import BV.Core.Types

data Result
  = Pass
  | Fail
  deriving (Eq, Generic, Ord, Show)

data IntermediateArtifact
  = IntermediateArtifactFunctions Program
  | IntermediateArtifactPairings Pairings
  | IntermediateArtifactProblems Problems
  | IntermediateArtifactProofChecks (ProofChecks String)
  | IntermediateArtifactSmtProofChecks (SmtProofChecks ())
  deriving (Eq, Generic, Ord, Show)

class Monad m => MonadRegisterIntermediateArtifacts m where
    registerIntermediateArtifact :: IntermediateArtifact -> m ()

class Monad m => MonadCache m where
    queryCache :: Pairings -> PairingId -> ProblemAndProof -> m (Maybe Result)

class Monad m => MonadSneakyIO m where
    liftSneakyIO :: IO () -> m ()

class (Monad m, MonadSolver n) => MonadSolvers n m | m -> n where
    liftIntoSolver :: m a -> n a
    withOnlineSolver :: (OnlineSolverConfig -> n a) -> m a
    withOfflineSolvers :: (OfflineSolverConfig -> n a) -> [m a]

data OnlineSolverConfig
  = OnlineSolverConfig
      { common :: CommonSolverConfig
      }
  deriving (Eq, Generic, Ord, Show)

data OfflineSolverConfig
  = OfflineSolverConfig
      { common :: CommonSolverConfig
      }
  deriving (Eq, Generic, Ord, Show)

data CommonSolverConfig
  = CommonSolverConfig
      { memoryMode :: SolverMemoryMode
      }
  deriving (Eq, Generic, Ord, Show)

data SolverMemoryMode
  = SolverMemoryModeWord8
  | SolverMemoryModeWord32
  deriving (Eq, Generic, Ord, Show)

class ( Monad m
      , MonadLogger m
      , MonadRegisterIntermediateArtifacts m
      , MonadSneakyIO m
      ) => MonadCheckWriteOnly m where

class ( Monad m
      , MonadCheckWriteOnly m
      , MonadCache m
      , MonadSolvers n m
      , MonadSolver n
      ) => MonadCheck n m where
