module BV.System.Utils.Logger
    ( MonadLogger
    , MonadLoggerContextStack (..)
    , addLoggerContextToStr
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

import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LogLevel (..), LogSource, LogStr,
                             LoggingT (LoggingT, runLoggingT), MonadLogger,
                             ToLogStr, logDebugN, logErrorN, logInfoN,
                             logOtherN, logWarnN, logWithoutLoc, toLogStr)
import Control.Monad.Reader (ReaderT, mapReaderT)
import qualified Data.Text as T

class MonadLogger m => MonadLoggerContextStack m where
    pushLogContext :: String -> m a -> m a

instance MonadIO m => MonadLoggerContextStack (LoggingT m) where
    pushLogContext ctx m =
        LoggingT $ \logger ->
            runLoggingT m $ \loc source level str ->
                logger loc source level (addLoggerContextToStr ctx str)

instance MonadLoggerContextStack m => MonadLoggerContextStack (ReaderT r m) where
    pushLogContext = mapReaderT . pushLogContext

instance MonadLoggerContextStack m => MonadLoggerContextStack (ExceptT e m) where
    pushLogContext = mapExceptT . pushLogContext

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
