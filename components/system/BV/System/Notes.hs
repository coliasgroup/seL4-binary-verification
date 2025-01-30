{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.System.Notes
    (
    ) where

import BV.Core.Types

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import GHC.Generics (Generic)

data Report
  = Report
  deriving (Eq, Generic, Ord, Show)

executeSMTProofChecks :: MonadExecuteSMTProofChecks m => SMTProofChecks String -> m Report
executeSMTProofChecks = undefined

data Result
  = Pass
  | Fail
  deriving (Eq, Generic, Ord, Show)

class ( Monad m
      , MonadLogger m
      , MonadCache m
      , MonadIO m
      ) => MonadExecuteSMTProofChecks m where

class Monad m => MonadCache m where
    queryCache :: SMTProofCheck () -> m (Maybe Result)

data SolverScope
  = SolverScopeHyp
  | SolverScopeAll
  deriving (Eq, Generic, Ord, Show)
