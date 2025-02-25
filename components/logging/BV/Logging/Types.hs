{-# LANGUAGE OverloadedStrings #-}

module BV.Logging.Types
    ( LogContext
    , LogContextEntry (..)
    , LogEntry (..)
    , LoggingWithContextEnv (..)
    , LoggingWithContextT (..)
    , MonadLogger
    , MonadLoggerWithContext (..)
    , defaultLoc
    , defaultLogSource
    , isDefaultLoc
    , isDefaultLogSource
    , makeLogContextEntry
    , mapLoggingWithContextT
    , runLoggingWithContextT
    , withPushLogContext
    ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (Loc, LogLevel (..), LogSource, LogStr,
                             MonadLogger (monadLoggerLog), defaultLoc, toLogStr)
import Control.Monad.Reader (MonadTrans, ReaderT, mapReaderT, runReaderT,
                             withReaderT)
import Data.Aeson.Types
import Data.Binary (Binary)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Optics (ViewableOptic (gview), (%~), (.~))
import Text.Printf (printf)

class MonadLogger m => MonadLoggerWithContext m where
    withPushLogContexts :: [String] -> m a -> m a
    withCleanLogContext :: m a -> m a

withPushLogContext :: MonadLoggerWithContext m => String -> m a -> m a
withPushLogContext entry = withPushLogContexts [entry]

instance MonadLoggerWithContext m => MonadLoggerWithContext (ReaderT r m) where
    withPushLogContexts = mapReaderT . withPushLogContexts
    withCleanLogContext = mapReaderT withCleanLogContext

instance MonadLoggerWithContext m => MonadLoggerWithContext (ExceptT e m) where
    withPushLogContexts = mapExceptT . withPushLogContexts
    withCleanLogContext = mapExceptT withCleanLogContext

--

data LogEntry
  = LogEntry
      { context :: LogContext
      , loc :: Loc
      , source :: LogSource
      , level :: LogLevel
      , msg :: LogStr
      }
  deriving (Eq, Generic, Show)

type LogContext = [LogContextEntry]

newtype LogContextEntry
  = LogContextEntry { unwrap :: String }
  deriving (Eq, FromJSON, Generic, Ord, Show, ToJSON)

instance Binary LogContextEntry where

makeLogContextEntry :: String -> LogContextEntry
makeLogContextEntry s =
    if ']' `elem` s
    then error $ printf "log context entry may not contain ']' (offender: \"%s\")" (show s)
    else LogContextEntry s

newtype LoggingWithContextT m a
  = LoggingWithContextT { unwrap :: ReaderT LoggingWithContextEnv m a }
  deriving
    ( Applicative
    , Functor
    , Generic
    , Monad
    , MonadCatch
    , MonadFail
    , MonadIO
    , MonadMask
    , MonadThrow
    , MonadTrans
    , MonadUnliftIO
    )

type LogAction = LogEntry -> IO ()

data LoggingWithContextEnv
  = LoggingWithContextEnv
      { context :: LogContext
      , logAction :: LogAction
      }
  deriving (Generic)

instance MonadIO m => MonadLogger (LoggingWithContextT m) where
    monadLoggerLog loc source level msg = LoggingWithContextT $ do
        context <- gview #context
        logAction <- gview #logAction
        let entry = LogEntry
                { context
                , loc
                , source
                , level
                , msg = toLogStr msg
                }
        liftIO $ logAction entry

instance MonadIO m => MonadLoggerWithContext (LoggingWithContextT m) where
    withPushLogContexts entries m = LoggingWithContextT $
        withReaderT (#context %~ (++ map makeLogContextEntry entries)) m.unwrap
    withCleanLogContext m = LoggingWithContextT $
        withReaderT (#context .~ []) m.unwrap

runLoggingWithContextT :: MonadIO m => LoggingWithContextT m a -> LogAction -> m a
runLoggingWithContextT m logAction =
    runReaderT
        m.unwrap
        LoggingWithContextEnv
            { context = []
            , logAction
            }

mapLoggingWithContextT :: (m a -> n b) -> LoggingWithContextT m a -> LoggingWithContextT n b
mapLoggingWithContextT f = #unwrap %~ mapReaderT f

--

isDefaultLoc :: Loc -> Bool
isDefaultLoc = (==) defaultLoc

isDefaultLogSource :: LogSource -> Bool
isDefaultLogSource = T.null

defaultLogSource :: LogSource
defaultLogSource = T.empty
