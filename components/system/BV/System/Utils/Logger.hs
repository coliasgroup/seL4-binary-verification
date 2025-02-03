module BV.System.Utils.Logger
    ( MonadLogger
    , MonadLoggerAddContext (..)
    , levelTrace
    , logDebug
    , logDebugGeneric
    , logError
    , logErrorGeneric
    , logInfo
    , logInfoGeneric
    , logTrace
    , logTraceGeneric
    , logWarn
    , logWarnGeneric
    , noTrace
    , noTraceAnd
    , addLoggerContextToStr
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LogLevel (..), LogSource,
                             LoggingT (LoggingT, runLoggingT), MonadLogger,
                             ToLogStr, logDebugN, logErrorN, logInfoN,
                             logOtherN, logWarnN, logWithoutLoc, toLogStr, LogStr)
import qualified Data.Text as T
import Control.Monad.Reader (ReaderT, mapReaderT)

class MonadLogger m => MonadLoggerAddContext m where
    addLoggerContext :: String -> m a -> m a

instance MonadIO m => MonadLoggerAddContext (LoggingT m) where
    addLoggerContext ctx m =
        LoggingT $ \logger ->
            runLoggingT m $ \loc source level str ->
                logger loc source level (addLoggerContextToStr ctx str)

instance MonadLoggerAddContext m => MonadLoggerAddContext (ReaderT r m) where
    addLoggerContext = mapReaderT . addLoggerContext

addLoggerContextToStr :: ToLogStr a => String -> a -> LogStr
addLoggerContextToStr ctx str = toLogStr ("[" ++ ctx ++ "] ") <> toLogStr str

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

levelTrace :: LogLevel
levelTrace = LevelOther (T.pack "Trace")

noTrace :: LogSource -> LogLevel -> Bool
noTrace _source level = level /= levelTrace

noTraceAnd :: (LogSource -> LogLevel -> Bool) -> LogSource -> LogLevel -> Bool
noTraceAnd p source level = noTrace source level && p source level
