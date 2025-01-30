module BV.System.SolversConfig
    ( OfflineSolverConfig (..)
    , OnlineSolverConfig (..)
    , SolverScope (..)
    , SolversConfig (..)
    ) where

import BV.Core.ExecuteSMTProofChecks

import GHC.Generics (Generic)

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
