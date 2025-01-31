module BV.System.SolversConfig
    ( OfflineSolverConfig (..)
    , OfflineSolverGroupConfig (..)
    , OnlineSolverConfig (..)
    , SolverScope (..)
    , SolversConfig (..)
    , allSolverMemoryModes
    , allSolverScopes
    , numOfflineSolverConfigs
    , offlineSolverConfigs
    ) where

import BV.Core.ExecuteSMTProofChecks

import Data.List (genericLength)
import GHC.Generics (Generic)

data SolversConfig
  = SolversConfig
      { online :: OnlineSolverConfig
      , onlineTimeout :: Integer
      , offline :: [OfflineSolverGroupConfig]
      , offlineTimeout :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

data OnlineSolverConfig
  = OnlineSolverConfig
      { command :: [String]
      , memoryMode :: SolverMemoryMode
      }
  deriving (Eq, Generic, Ord, Show)

data OfflineSolverGroupConfig
  = OfflineSolverGroupConfig
      { command :: [String]
      , memoryModes :: [SolverMemoryMode]
      , scopes :: [SolverScope]
      }
  deriving (Eq, Generic, Ord, Show)

data SolverScope
  = SolverScopeHyp
  | SolverScopeAll
  deriving (Eq, Generic, Ord, Show)

allSolverScopes :: [SolverScope]
allSolverScopes = [SolverScopeHyp, SolverScopeAll]

allSolverMemoryModes :: [SolverMemoryMode]
allSolverMemoryModes = [SolverMemoryModeWord8, SolverMemoryModeWord32]

data OfflineSolverConfig
  = OfflineSolverConfig
      { command :: [String]
      , memoryMode :: SolverMemoryMode
      , scope :: SolverScope
      }
  deriving (Eq, Generic, Ord, Show)

offlineSolverConfigs :: SolversConfig -> [OfflineSolverConfig]
offlineSolverConfigs (SolversConfig { offline }) = flip concatMap offline $
    \OfflineSolverGroupConfig { command, memoryModes, scopes } ->
        OfflineSolverConfig command <$> memoryModes <*> scopes

numOfflineSolverConfigs :: SolversConfig -> Integer
numOfflineSolverConfigs = genericLength . offlineSolverConfigs
