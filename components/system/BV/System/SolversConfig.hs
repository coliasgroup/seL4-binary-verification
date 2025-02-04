module BV.System.SolversConfig
    ( OfflineSolverConfig (..)
    , OfflineSolverGroupConfig (..)
    , OnlineSolverConfig (..)
    , SolverMemoryMode (..)
    , SolverScope (..)
    , SolversConfig (..)
    , allSolverMemoryModes
    , allSolverScopes
    , numOfflineSolverConfigsForScope
    , offlineSolverConfigsForScope
    ) where

import BV.Core.ExecuteSMTProofChecks

import Control.Applicative (empty)
import Control.Monad (unless)
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
      { name :: String
      , command :: [String]
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
      }
  deriving (Eq, Generic, Ord, Show)

offlineSolverConfigsForScope :: SolverScope -> SolversConfig -> [OfflineSolverConfig]
offlineSolverConfigsForScope scope (SolversConfig { offline }) = flip concatMap offline $
    \OfflineSolverGroupConfig { command, memoryModes, scopes } -> do
        unless (scope `elem` scopes) empty
        OfflineSolverConfig command <$> memoryModes

numOfflineSolverConfigsForScope :: SolverScope ->  SolversConfig -> Integer
numOfflineSolverConfigsForScope scope = genericLength . offlineSolverConfigsForScope scope
