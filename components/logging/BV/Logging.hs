module BV.Logging
    ( LogContext
    , LogContextEntry (..)
    , LogEntry (..)
    , LogLevel (..)
    , LogLevelWithTrace (..)
    , LoggingWithContextT
    , MonadLogger
    , MonadLoggerWithContext (..)
    , formatLogEntryHuman
    , formatLogEntryJSON
    , formatLogEntryText
    , levelAtLeastWithTrace
    , levelTrace
    , logDebug
    , logDebugGeneric
    , logEntryWithContext
    , logError
    , logErrorGeneric
    , logInfo
    , logInfoGeneric
    , logTrace
    , logTraceGeneric
    , logWarn
    , logWarnGeneric
    , mapLoggingWithContextT
    , parseLogEntryHumanBestEffort
    , parseLogEntryJSON
    , parseLogEntryText
    , runLoggingWithContextT
    , runSimpleLoggingWithContextT
    , simpleLogOutput
    , withPushLogContext
    ) where

import BV.Logging.Adapters
import BV.Logging.Aeson ()
import BV.Logging.Binary ()
import BV.Logging.Formatting
import BV.Logging.LevelWithTrace
import BV.Logging.Parsing
import BV.Logging.Types

import Control.Monad (when)
import Control.Monad.Logger (LogLevel (..), ToLogStr, logDebugN, logErrorN,
                             logInfoN, logOtherN, logWarnN, logWithoutLoc,
                             toLogStr)
import Data.ByteString.Builder (Builder, hPutBuilder)
import qualified Data.Text as T
import System.IO (Handle)

logTrace :: MonadLogger m => String -> m ()
logTrace = logOtherN levelTrace . T.pack

logDebug :: MonadLogger m => String -> m ()
logDebug = logDebugN . T.pack

logInfo :: MonadLogger m => String -> m ()
logInfo = logInfoN . T.pack

logWarn :: MonadLogger m => String -> m ()
logWarn = logWarnN . T.pack

logError :: MonadLogger m => String -> m ()
logError = logErrorN . T.pack

logGeneric :: (MonadLogger m, ToLogStr a) => LogLevel -> a -> m ()
logGeneric level = logWithoutLoc T.empty level . toLogStr

logTraceGeneric :: (MonadLogger m, ToLogStr a) => a -> m ()
logTraceGeneric = logGeneric levelTrace

logInfoGeneric :: (MonadLogger m, ToLogStr a) => a -> m ()
logInfoGeneric = logGeneric LevelInfo

logDebugGeneric :: (MonadLogger m, ToLogStr a) => a -> m ()
logDebugGeneric = logGeneric LevelDebug

logWarnGeneric :: (MonadLogger m, ToLogStr a) => a -> m ()
logWarnGeneric = logGeneric LevelWarn

logErrorGeneric :: (MonadLogger m, ToLogStr a) => a -> m ()
logErrorGeneric = logGeneric LevelError

simpleLogOutput :: (LogEntry -> Builder) -> LogLevel -> Handle -> LogEntry -> IO ()
simpleLogOutput f minLevel h entry =
    when (levelAtLeastWithTrace minLevel entry.level) $ hPutBuilder h (f entry)
