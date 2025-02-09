module BV.System.SolversConfig
    ( OfflineSolverConfig (..)
    , OfflineSolverGroupConfig (..)
    , OfflineSolverName
    , OfflineSolversConfig (..)
    , OnlineSolverConfig (..)
    , SolverMemoryMode (..)
    , SolverScope (..)
    , SolversConfig (..)
    , allModelConfigs
    , allSolverScopes
    , numOfflineSolverConfigsForScope
    , offlineSolverConfigsForScope
    , prettySolverMemoryMode
    , prettySolverScope
    ) where

import BV.Core.ExecuteSMTProofChecks
import BV.SMTLIB2.Monad

import Control.Applicative (empty)
import Control.Monad (unless)
import Data.List (genericLength)
import GHC.Generics (Generic)

data SolversConfig
  = SolversConfig
      { online :: Maybe OnlineSolverConfig
      , offline :: OfflineSolversConfig
      }
  deriving (Eq, Generic, Ord, Show)

data OnlineSolverConfig
  = OnlineSolverConfig
      { command :: [String]
      , modelConfig :: ModelConfig
      , timeout :: SolverTimeout
      }
  deriving (Eq, Generic, Ord, Show)

data OfflineSolversConfig
  = OfflineSolversConfig
      { groups :: [OfflineSolverGroupConfig]
      , timeout :: SolverTimeout
      }
  deriving (Eq, Generic, Ord, Show)

type OfflineSolverName = String

data OfflineSolverGroupConfig
  = OfflineSolverGroupConfig
      { commandName :: OfflineSolverName
      , command :: [String]
      , scopes :: [SolverScope]
      , modelConfigs :: [ModelConfig]
      }
  deriving (Eq, Generic, Ord, Show)

data SolverScope
  = SolverScopeHyp
  | SolverScopeAll
  deriving (Eq, Generic, Ord, Show)

allSolverScopes :: [SolverScope]
allSolverScopes = [SolverScopeHyp, SolverScopeAll]

allModelConfigs :: [ModelConfig]
allModelConfigs = do
    memoryMode <- [SolverMemoryModeWord8, SolverMemoryModeWord32]
    return $ ModelConfig { memoryMode }

data OfflineSolverConfig
  = OfflineSolverConfig
      { commandName :: String
      , command :: [String]
      , config :: ModelConfig
      }
  deriving (Eq, Generic, Ord, Show)

offlineSolverConfigsForScope :: SolverScope -> [OfflineSolverGroupConfig] -> [OfflineSolverConfig]
offlineSolverConfigsForScope scope groups = flip concatMap groups $
    \OfflineSolverGroupConfig { commandName, command, scopes, modelConfigs } -> do
        unless (scope `elem` scopes) empty
        OfflineSolverConfig commandName command <$> modelConfigs

numOfflineSolverConfigsForScope :: SolverScope ->  [OfflineSolverGroupConfig] -> Integer
numOfflineSolverConfigsForScope scope = genericLength . offlineSolverConfigsForScope scope

prettySolverScope :: SolverScope -> String
prettySolverScope = \case
    SolverScopeAll -> "all"
    SolverScopeHyp -> "hyp"
