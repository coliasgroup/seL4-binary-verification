{-# LANGUAGE OverloadedStrings #-}

module BV.Logging.LevelWithTrace
    ( LogLevelWithTrace (..)
    , levelAtLeastWithTrace
    , levelTrace
    ) where

import Control.Monad.Logger (LogLevel (..))
import Data.Function (on)
import qualified Data.Text as T
import GHC.Generics (Generic)


levelTrace :: LogLevel
levelTrace = LevelOther (T.pack "Trace")

newtype LogLevelWithTrace
  = LogLevelWithTrace { unwrap :: LogLevel }
  deriving (Eq, Generic, Show)

compareWithTrace :: LogLevel -> LogLevel -> Ordering
compareWithTrace (LevelOther "Trace") (LevelOther "Trace") = EQ
compareWithTrace _ (LevelOther "Trace") = GT
compareWithTrace (LevelOther "Trace") _ = LT
compareWithTrace a b = compare a b

instance Ord LogLevelWithTrace where
    compare = compareWithTrace `on` (.unwrap)

levelAtLeastWithTrace :: LogLevel -> LogLevel -> Bool
levelAtLeastWithTrace = (<=) `on` LogLevelWithTrace
