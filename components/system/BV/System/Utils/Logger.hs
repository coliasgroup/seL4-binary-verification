{-# LANGUAGE OverloadedStrings #-}

module BV.System.Utils.Logger
    ( LoggingWithContextT
    , MonadLogger
    , MonadLoggerWithContext (..)
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
    , runSimpleLoggingWithContextT
    ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (Loc, LogLevel (..), LogSource, LogStr,
                             LoggingT (LoggingT), MonadLogger (monadLoggerLog),
                             ToLogStr, logDebugN, logErrorN, logInfoN,
                             logOtherN, logWarnN, logWithoutLoc, toLogStr)
import Control.Monad.Reader (ReaderT, mapReaderT, runReaderT, withReaderT)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Optics (ViewableOptic (gview), (%~))

class MonadLogger m => MonadLoggerWithContext m where
    pushLogContext :: String -> m a -> m a

instance MonadLoggerWithContext m => MonadLoggerWithContext (ReaderT r m) where
    pushLogContext = mapReaderT . pushLogContext

instance MonadLoggerWithContext m => MonadLoggerWithContext (ExceptT e m) where
    pushLogContext = mapExceptT . pushLogContext

type LogContextEntry = String

type LogContext = [LogContextEntry]

newtype LoggingWithContextT m a
  = LoggingWithContextT { unwrap :: ReaderT LoggingWithContextEnv m a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadFail
    , MonadIO
    , MonadMask
    , MonadThrow
    , MonadUnliftIO
    )

data LoggingWithContextEnv
  = LoggingWithContextEnv
      { context :: LogContext
      , logAction :: LogContext -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
      }
  deriving (Generic)

runSimpleLoggingWithContextT :: MonadIO m => LoggingWithContextT m a -> LoggingT m a
runSimpleLoggingWithContextT m = LoggingT $ \logAction ->
    runReaderT
        m.unwrap
        LoggingWithContextEnv
            { context = []
            , logAction = \context loc source level msg ->
                logAction loc source level (showContext context <> " " <> msg)
            }
  where
    showContext = foldMap (\ctx -> "[" <> toLogStr ctx <> "] ")

instance MonadIO m => MonadLogger (LoggingWithContextT m) where
    monadLoggerLog loc source level msg = LoggingWithContextT $ do
        context <- gview #context
        logAction <- gview #logAction
        liftIO $ logAction context loc source level (toLogStr msg)

instance MonadIO m => MonadLoggerWithContext (LoggingWithContextT m) where
    pushLogContext entry m = LoggingWithContextT $ withReaderT (#context %~ (++ [entry])) m.unwrap

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
