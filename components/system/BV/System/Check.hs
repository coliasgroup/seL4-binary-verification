module BV.System.Check
    ( MonadCache (..)
    , executeChecks
    ) where

import BV.Core.AdornProofScript
import BV.Core.ExecuteSMTProofChecks
import BV.Core.Types
import BV.SMTLIB2.Types.Command

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import qualified Data.Map as M
import GHC.Generics (Generic)

data ExecuteChecksConfig
  = ExecuteChecksConfig
      { numCores :: Integer
      , solversConfig :: SolversConfig
      }
  deriving (Eq, Generic, Ord, Show)

data SolversConfig
  = SolversConfig
      { online :: OnlineSolverConfig
      , offline :: [OfflineSolverConfig]
      }
  deriving (Eq, Generic, Ord, Show)

data OnlineSolverConfig
  = OnlineSolverConfig
      { command :: [String]
      , memoryMode :: SolverMemoryMode
      }
  deriving (Eq, Generic, Ord, Show)

data OfflineSolverConfig
  = OfflineSolverConfig
      { command :: [String]
      , memoryModes :: [SolverMemoryMode]
      , scopes :: [SolverScope]
      }
  deriving (Eq, Generic, Ord, Show)

data SolverScope
  = SolverScopeHyp
  | SolverScopeAll
  deriving (Eq, Generic, Ord, Show)

data Report
  = Report
      { unwrap :: M.Map PairingId ProblemCheckResult
      }
  deriving (Eq, Generic, Ord, Show)

type ProblemCheckResult = Maybe (ProblemCheckError, [ProofScriptNodeLocation])

data ProblemCheckError
  = NoSolversAnswered
  | SomeSolverAnsweredSat
  | AllSolversAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

class Monad m => MonadCache m where
    queryCache :: SMTProofCheck () -> m (Maybe AcceptableSatResult)
    updateCache :: SMTProofCheck () -> AcceptableSatResult -> m ()

data AcceptableSatResult
  = AcceptableSatResultSat
  | AcceptableSatResultUnsat
  deriving (Eq, Generic, Ord, Show)

executeChecks
    :: (MonadIO m, MonadLogger m, MonadCache m)
    => FlattenedSMTProofChecks String -> m Report
executeChecks = undefined
