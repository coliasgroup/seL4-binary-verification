{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.ExecuteSmtProofChecks
    ( executeSmtProofChecks
    ) where

import Control.Monad.Logger (MonadLogger)
import GHC.Generics (Generic)

import BV.SMTLIB2.Types

import BV.Core.ConfigureSmt
import BV.Core.Types

data Report
  = Report
  deriving (Eq, Generic, Ord, Show)

executeSmtProofChecks :: MonadSolvers n m => SmtProofChecks String -> m Report
executeSmtProofChecks = undefined

data Result
  = Pass
  | Fail
  deriving (Eq, Generic, Ord, Show)

class ( Monad m
      , MonadLogger m
      , MonadCache m
      , MonadSolvers n m
      ) => MonadExecuteSmtProofChecks n m where

class Monad m => MonadCache m where
    queryCache :: SmtProofCheckGroup () -> m (Maybe Result)

class (Monad m, MonadLogger m, MonadSolver n) => MonadSolvers n m | m -> n where
    liftIntoSolver :: m a -> n a
    withOnlineSolver :: (OnlineSolverConfig -> n a) -> m a
    withOfflineSolvers :: (OfflineSolverConfig -> n a) -> [m a]

data SolverScope
  = SolverScopeHyp
  | SolverScopeAll
  deriving (Eq, Generic, Ord, Show)

data OnlineSolverConfig
  = OnlineSolverConfig
      { common :: SolverConfig
      }
  deriving (Eq, Generic, Ord, Show)

data OfflineSolverConfig
  = OfflineSolverConfig
      { common :: SolverConfig
      }
  deriving (Eq, Generic, Ord, Show)
