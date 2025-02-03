module BV.System.Utils.Logger
    ( MonadLogger
    , MonadLoggerIO
    , addLogContext
    , addLogContext'
    , addLogContextToStr
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
    ) where

import Control.Monad.Logger (LogLevel (..), LogSource, LogStr,
                             LoggingT (LoggingT, runLoggingT), MonadLogger,
                             MonadLoggerIO (askLoggerIO), ToLogStr (toLogStr),
                             logDebugN, logErrorN, logInfoN, logOtherN,
                             logWarnN, logWithoutLoc)
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
addLogContext ctx = prependToLogs $ "[" ++ ctx ++ "] "

addLogContext' :: MonadLoggerIO m => String -> LoggingT m a -> m a
addLogContext' ctx = prependToLogs' $ "[" ++ ctx ++ "] "

addLogContextToStr :: ToLogStr a => String -> a -> LogStr
addLogContextToStr ctx str = toLogStr ("[" ++ ctx ++ "] ") <> toLogStr str

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
