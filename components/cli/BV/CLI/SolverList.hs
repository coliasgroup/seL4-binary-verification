{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.CLI.SolverList
    ( SolverList (..)
    , SolverListOfflineSolverGroup (..)
    , SolverListOnlineSolver (..)
    ) where

import BV.Core.ExecuteSMTProofChecks
import BV.System.SolversConfig

import Data.Aeson

import Data.Aeson.Types (Parser)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics (Generic)

data SolverList
  = SolverList
      { online :: Maybe SolverListOnlineSolver
      , offline :: M.Map OfflineSolverName SolverListOfflineSolverGroup
      }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON SolverList where

instance FromJSON SolverList where
    parseJSON = withObject "SolverList" $ \v -> SolverList
        <$> v .:? "online"
        <*> (v .:? "offline") .!= M.empty

data SolverListOnlineSolver
  = SolverListOnlineSolver
      { command :: [String]
      , memoryMode :: SolverMemoryMode
      }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON SolverListOnlineSolver where
    toJSON v = object
        [ "command" .= v.command
        , "memory_mode" .= encodeMemoryMode v.memoryMode
        ]

instance FromJSON SolverListOnlineSolver where
    parseJSON = withObject "SolverListOnlineSolver" $ \v -> SolverListOnlineSolver
        <$> v .: "command"
        <*> ((v .:? "memory_mode") >>= traverse decodeMemoryMode) .!= defaultMemoryMode

data SolverListOfflineSolverGroup
  = SolverListOfflineSolverGroup
      { command :: [String]
      , memoryModes :: [SolverMemoryMode]
      , scopes :: [SolverScope]
      }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON SolverListOfflineSolverGroup where
    toJSON v = object
        [ "command" .= v.command
        , "memory_modes" .= map encodeMemoryMode v.memoryModes
        , "scopes" .= map encodeSolverScope v.scopes
        ]

instance FromJSON SolverListOfflineSolverGroup where
    parseJSON = withObject "SolverListOfflineSolverGroup" $ \v -> SolverListOfflineSolverGroup
        <$> v .: "command"
        <*> ((v .:? "memory_modes") >>= (traverse . traverse) decodeMemoryMode) .!= defaultMemoryModes
        <*> ((v .:? "scopes") >>= (traverse . traverse) decodeSolverScope) .!= defaultSolverScopes

--

decodeMemoryMode :: Value -> Parser SolverMemoryMode
decodeMemoryMode = withText "SolverMemoryMode" $ \case
    "word8" -> pure SolverMemoryModeWord8
    "word32" -> pure SolverMemoryModeWord32
    _ -> fail "unrecognized memory mode"

encodeMemoryMode :: SolverMemoryMode -> Value
encodeMemoryMode = String . T.pack . prettySolverMemoryMode

defaultMemoryMode :: SolverMemoryMode
defaultMemoryMode = SolverMemoryModeWord32

defaultMemoryModes :: [SolverMemoryMode]
defaultMemoryModes = [defaultMemoryMode]

decodeSolverScope :: Value -> Parser SolverScope
decodeSolverScope = withText "SolverScope" $ \case
    "all" -> pure SolverScopeAll
    "hyp" -> pure SolverScopeHyp
    _ -> fail "unrecognized solver scope"

encodeSolverScope :: SolverScope -> Value
encodeSolverScope = String . T.pack . prettySolverScope

defaultSolverScopes :: [SolverScope]
defaultSolverScopes = allSolverScopes
