{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.ExecuteSMTProofChecks
    ( executeSMTProofChecks
    ) where

import Control.Monad.Logger (MonadLogger)
import GHC.Generics (Generic)

import BV.SMTLIB2.Types

import BV.Core.ConfigureSMT
import BV.Core.Types

data Report
  = Report
  deriving (Eq, Generic, Ord, Show)

executeSMTProofChecks :: MonadSolvers n m => SMTProofChecks String -> m Report
executeSMTProofChecks = undefined

data Result
  = Pass
  | Fail
  deriving (Eq, Generic, Ord, Show)

class ( Monad m
      , MonadLogger m
      , MonadCache m
      , MonadSolvers n m
      ) => MonadExecuteSMTProofChecks n m where

class Monad m => MonadCache m where
    queryCache :: SMTProofCheck () -> m (Maybe Result)

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
