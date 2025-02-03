module BV.System.Utils.Logger
    ( MonadLogger
    , MonadLoggerIO
    , addLogContext
    , addLogContext'
    , levelTrace
    , logDebug
    , logError
    , logInfo
    , logTrace
    , logWarn
    , noTrace
    , noTraceAnd
    , prependToLogs
    , prependToLogs'
    ) where

import Control.Monad.Logger (LogLevel (LevelOther), LogSource,
                             LoggingT (LoggingT, runLoggingT), MonadLogger,
                             MonadLoggerIO (askLoggerIO), ToLogStr (toLogStr),
                             logDebugN, logErrorN, logInfoN, logOtherN,
                             logWarnN)
import qualified Data.Text as T

prependToLogs :: String -> LoggingT m a -> LoggingT m a
prependToLogs prefix m =
    LoggingT $ \logger ->
        runLoggingT m $ \loc source level str ->
            logger loc source level (toLogStr prefix <> str)

prependToLogs' :: MonadLoggerIO m => String -> LoggingT m a -> m a
prependToLogs' prefix m = do
    logger <- askLoggerIO
    runLoggingT m $
        \loc source level str ->
            logger loc source level (toLogStr prefix <> str)

addLogContext :: String -> LoggingT m a -> LoggingT m a
addLogContext prefix = prependToLogs $ "[" ++ prefix ++ "] "

addLogContext' :: MonadLoggerIO m => String -> LoggingT m a -> m a
addLogContext' prefix = prependToLogs' $ "[" ++ prefix ++ "] "

logDebug :: MonadLogger m => String -> m ()
logDebug = logDebugN . T.pack

logInfo :: MonadLogger m => String -> m ()
logInfo = logInfoN . T.pack

logWarn :: MonadLogger m => String -> m ()
logWarn = logWarnN . T.pack

logError :: MonadLogger m => String -> m ()
logError = logErrorN . T.pack

logTrace :: MonadLogger m => String -> m ()
logTrace = logOtherN levelTrace . T.pack

levelTrace :: LogLevel
levelTrace = LevelOther (T.pack "Trace")

noTrace :: LogSource -> LogLevel -> Bool
noTrace _source level = level /= levelTrace

noTraceAnd :: (LogSource -> LogLevel -> Bool) -> LogSource -> LogLevel -> Bool
noTraceAnd p source level = noTrace source level && p source level
