{-# LANGUAGE OverloadedStrings #-}

module BV.Logging.LevelWithTrace
    ( LogLevelWithTrace (..)
    , levelAtLeastWithTrace
    , levelTrace
    ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (Loc, LogLevel (..), LogSource, LogStr,
                             MonadLogger (monadLoggerLog), toLogStr)
import Control.Monad.Reader (ReaderT, mapReaderT, runReaderT, withReaderT)
import Data.Aeson.Types
import Data.Function (on)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Optics (ViewableOptic (gview), (%~), (.~))
import Text.Printf (printf)


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
