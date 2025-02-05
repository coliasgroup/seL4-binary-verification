{-# LANGUAGE OverloadedStrings #-}

module BV.System.Utils.Logger
    ( LoggingWithContextStackT (LoggingWithContextStackT)
    , MonadLogger
    , MonadLoggerWithContextStack (..)
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
    , runSimpleLoggingWithContextStackT
    ) where

import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (Loc, LogLevel (..), LogSource, LogStr,
                             LoggingT (LoggingT, runLoggingT),
                             MonadLogger (monadLoggerLog), ToLogStr, logDebugN,
                             logErrorN, logInfoN, logOtherN, logWarnN,
                             logWithoutLoc, toLogStr)
import Control.Monad.Reader (ReaderT, mapReaderT, runReaderT, withReaderT)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Optics (ViewableOptic (gview), (%~))

class MonadLogger m => MonadLoggerWithContextStack m where
    pushLogContext :: String -> m a -> m a

instance MonadLoggerWithContextStack m => MonadLoggerWithContextStack (ReaderT r m) where
    pushLogContext = mapReaderT . pushLogContext

instance MonadLoggerWithContextStack m => MonadLoggerWithContextStack (ExceptT e m) where
    pushLogContext = mapExceptT . pushLogContext

type LogContextEntry = String

type LogContextStack = [LogContextEntry]

newtype LoggingWithContextStackT m a
  = LoggingWithContextStackT { unwrap :: ReaderT LoggingWithContextStackTContext m a }
  deriving (Applicative, Functor, Monad)

data LoggingWithContextStackTContext
  = LoggingWithContextStackTContext
      { contextStack :: LogContextStack
      , logAction :: LogContextStack -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
      }
  deriving (Generic)

runSimpleLoggingWithContextStackT :: MonadIO m => LoggingWithContextStackT m a -> LoggingT m a
runSimpleLoggingWithContextStackT m = LoggingT $ \logAction ->
    runReaderT
        m.unwrap
        LoggingWithContextStackTContext
            { contextStack = []
            , logAction = \contextStack loc source level msg ->
                logAction loc source level (showContextStack contextStack <> " " <> msg)
            }
  where
    showContextStack = foldMap (\ctx -> "[" <> toLogStr ctx <> "]")

instance MonadIO m => MonadLogger (LoggingWithContextStackT m) where
    monadLoggerLog loc source level msg = LoggingWithContextStackT $ do
        contextStack <- gview #contextStack
        logAction <- gview #logAction
        liftIO $ logAction contextStack loc source level (toLogStr msg)

instance MonadIO m => MonadLoggerWithContextStack (LoggingWithContextStackT m) where
    pushLogContext ctx m = LoggingWithContextStackT $ withReaderT (#contextStack %~ (++ [ctx])) m.unwrap

instance MonadIO m => MonadLoggerWithContextStack (LoggingT m) where
    pushLogContext ctx m =
        LoggingT $ \logger ->
            runLoggingT m $ \loc source level str ->
                logger loc source level (addLoggerContextToStr ctx str)

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
