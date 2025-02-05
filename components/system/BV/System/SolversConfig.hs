module BV.System.SolversConfig
    ( OfflineSolverConfig (..)
    , OfflineSolverGroupConfig (..)
    , OnlineSolverConfig (..)
    , SolverMemoryMode (..)
    , SolverScope (..)
    , SolversConfig (..)
    , allSolverScopes
    , allSolverSonfigs
    , numOfflineSolverConfigsForScope
    , offlineSolverConfigsForScope
    ) where

import BV.Core.ExecuteSMTProofChecks
import BV.SMTLIB2.Monad

import Control.Applicative (empty)
import Control.Monad (unless)
import Data.List (genericLength)
import GHC.Generics (Generic)

data SolversConfig
  = SolversConfig
      { online :: OnlineSolverConfig
      , onlineTimeout :: SolverTimeout
      , offline :: [OfflineSolverGroupConfig]
      , offlineTimeout :: SolverTimeout
      }
  deriving (Eq, Generic, Ord, Show)

data OnlineSolverConfig
  = OnlineSolverConfig
      { command :: [String]
      , config :: ModelConfig
      }
  deriving (Eq, Generic, Ord, Show)

data OfflineSolverGroupConfig
  = OfflineSolverGroupConfig
      { commandName :: String
      , command :: [String]
      , configs :: [ModelConfig]
      , scopes :: [SolverScope]
      }
  deriving (Eq, Generic, Ord, Show)

data SolverScope
  = SolverScopeHyp
  | SolverScopeAll
  deriving (Eq, Generic, Ord, Show)

allSolverScopes :: [SolverScope]
allSolverScopes = [SolverScopeHyp, SolverScopeAll]

allSolverSonfigs :: [ModelConfig]
allSolverSonfigs = do
    memoryMode <- [SolverMemoryModeWord8, SolverMemoryModeWord32]
    return $ ModelConfig { memoryMode }

data OfflineSolverConfig
  = OfflineSolverConfig
      { commandName :: String
      , command :: [String]
      , config :: ModelConfig
      }
  deriving (Eq, Generic, Ord, Show)

offlineSolverConfigsForScope :: SolverScope -> SolversConfig -> [OfflineSolverConfig]
offlineSolverConfigsForScope scope (SolversConfig { offline }) = flip concatMap offline $
    \OfflineSolverGroupConfig { commandName, command, configs, scopes } -> do
        unless (scope `elem` scopes) empty
        OfflineSolverConfig commandName command <$> configs

numOfflineSolverConfigsForScope :: SolverScope ->  SolversConfig -> Integer
numOfflineSolverConfigsForScope scope = genericLength . offlineSolverConfigsForScope scope
