{-# LANGUAGE OverloadedStrings #-}

module BV.CLI.WorkersConfig
    ( WorkerConfig (..)
    , WorkerName
    , WorkersConfig (..)
    ) where

import Data.Aeson

import Control.Applicative ((<|>))
import Data.Aeson.Types (explicitParseField)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as M
import GHC.Generics (Generic)

type WorkerName = String

data WorkersConfig
  = WorkersConfig
      { workers :: M.Map WorkerName WorkerConfig
      }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON WorkersConfig where

data WorkerConfig
  = WorkerConfig
      { command :: NonEmpty String
      , footprintOpt :: Maybe Integer
      , numCapabilitiesOpt :: Maybe Integer
      , numJobsOpt :: Maybe Integer
      , priority :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON WorkerConfig where
    parseJSON = withObject "WorkerConfig" $ \v -> WorkerConfig
        <$> (v .: "command" <|> explicitParseField localTrue v "local")
        <*> v .:? "footprint"
        <*> v .:? "num_capabilities"
        <*> v .:? "num_jobs"
        <*> v .:? "priority" .!= 0
      where
        localTrue = \case
            Bool True -> pure ("/proc/self/exe" :| [])
            _ -> fail "unexpected value"
