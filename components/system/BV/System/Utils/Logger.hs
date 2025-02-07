{-# LANGUAGE OverloadedStrings #-}

module BV.System.Utils.Logger
    ( LoggingWithContextT
    , MonadLogger
    , MonadLoggerWithContext (..)
    , filterLevelsBelow
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
                             ToLogStr, fromLogStr, logDebugN, logErrorN,
                             logInfoN, logOtherN, logWarnN, logWithoutLoc,
                             toLogStr)
import Control.Monad.Reader (ReaderT, mapReaderT, runReaderT, withReaderT)
import qualified Data.ByteString as B
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import GHC.Generics (Generic)
import Optics (ViewableOptic (gview), (%~))
import System.IO (Handle)

class MonadLogger m => MonadLoggerWithContext m where
    withPushLogContext :: String -> m a -> m a

instance MonadLoggerWithContext m => MonadLoggerWithContext (ReaderT r m) where
    withPushLogContext = mapReaderT . withPushLogContext

instance MonadLoggerWithContext m => MonadLoggerWithContext (ExceptT e m) where
    withPushLogContext = mapExceptT . withPushLogContext

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

instance MonadIO m => MonadLogger (LoggingWithContextT m) where
    monadLoggerLog loc source level msg = LoggingWithContextT $ do
        context <- gview #context
        logAction <- gview #logAction
        liftIO $ logAction context loc source level (toLogStr msg)

instance MonadIO m => MonadLoggerWithContext (LoggingWithContextT m) where
    withPushLogContext entry m =
        if ']' `elem` entry
        then error "log context entry may not contain ']'"
        else LoggingWithContextT $ withReaderT (#context %~ (++ [entry])) m.unwrap

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

filterLevelsBelow :: LogLevel -> LogSource -> LogLevel -> Bool
filterLevelsBelow minLevel _source level
  | minLevel == levelTrace = True
  | level == levelTrace = False
  | otherwise = level >= minLevel

--

runSimpleLoggingWithContextT :: MonadIO m => LoggingWithContextT m a -> LoggingT m a
runSimpleLoggingWithContextT m = LoggingT $ \logAction ->
    runReaderT
        m.unwrap
        LoggingWithContextEnv
            { context = []
            , logAction = \context loc source level msg ->
                logAction loc source level (msgWithContext context msg)
            }
  where
    msgWithContext context msg =
        -- LogStr is implemented as a bytestring builder, so a round trip is actually the most efficient here
        let msgBytes = fromLogStr (toLogStr msg)
         in foldMap (\ctx -> "[" <> toLogStr ctx <> "] ") context
            <> "(" <> toLogStr (show (B.length msgBytes)) <> ") "
            <> toLogStr msgBytes
