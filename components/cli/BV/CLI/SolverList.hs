{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.CLI.SolverList
    ( SolverList (..)
    , SolverListOfflineSolverGroup (..)
    , SolverListOnlineSolver (..)
    ) where

import BV.Core.Prelude
import BV.System.Core

import Data.Aeson

import Data.Aeson.Types (Parser)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics (Generic)

data SolverList
  = SolverList
      { online :: Maybe SolverListOnlineSolver
      , offline :: M.Map OfflineSolverCommandName SolverListOfflineSolverGroup
      }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON SolverList

instance FromJSON SolverList where
    parseJSON = withObject "SolverList" $ \v -> SolverList
        <$> v .:? "online"
        <*> (v .:? "offline") .!= M.empty

data SolverListOnlineSolver
  = SolverListOnlineSolver
      { command :: NonEmpty String
      , memoryMode :: MemoryMode
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
      { command :: NonEmpty String
      , memoryModes :: [MemoryMode]
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

decodeMemoryMode :: Value -> Parser MemoryMode
decodeMemoryMode = withText "MemoryMode" $ \case
    "word8" -> pure MemoryModeWord8
    "word32" -> pure MemoryModeWord32
    _ -> fail "unrecognized memory mode"

encodeMemoryMode :: MemoryMode -> Value
encodeMemoryMode = String . T.pack . prettyMemoryMode

defaultMemoryMode :: MemoryMode
defaultMemoryMode = MemoryModeWord32

defaultMemoryModes :: [MemoryMode]
defaultMemoryModes = [defaultMemoryMode]

decodeSolverScope :: Value -> Parser SolverScope
decodeSolverScope = withText "SolverScope" $ \case
    "all" -> pure SolverScopeAll
    "hyp" -> pure SolverScopeHyp
    _ -> fail "unrecognized solver scope"

encodeSolverScope :: SolverScope -> Value
encodeSolverScope = String . T.pack . prettySolverScope

defaultSolverScopes :: [SolverScope]
defaultSolverScopes = [minBound..maxBound]
