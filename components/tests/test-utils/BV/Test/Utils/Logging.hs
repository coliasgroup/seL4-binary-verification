module BV.Test.Utils.Logging
    ( FileLogOpts (..)
    , LogFormat (..)
    , LogLevel (..)
    , LogOpts (..)
    , LoggingOpts (..)
    , withLoggingOpts
    ) where

import BV.Logging

import Control.Concurrent (newMVar)
import Control.Concurrent.MVar (withMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.ByteString.Builder (Builder, hPutBuilder)
import GHC.Generics (Generic)
import System.IO (BufferMode (..), Handle, IOMode (..), hClose, hSetBuffering,
                  openFile, stderr)

data LoggingOpts
  = LoggingOpts
      { stderrLogOpts :: LogOpts
      , fileLogOpts :: Maybe FileLogOpts
      }
  deriving (Generic, Show)

data FileLogOpts
  = FileLogOpts
      { dst :: FilePath
      , logOpts :: LogOpts
      }
  deriving (Generic, Show)

data LogOpts
  = LogOpts
      { level :: LogLevel
      , format :: LogFormat
      }
  deriving (Generic, Show)

data LogFormat
  = LogFormatJSON
  | LogFormatText
  | LogFormatHuman
  deriving (Generic, Show)

logEntryFormatterFor :: LogFormat -> LogEntry -> Builder
logEntryFormatterFor = \case
    LogFormatJSON -> formatLogEntryJSON
    LogFormatText -> formatLogEntryText
    LogFormatHuman -> formatLogEntryHuman

withLoggingOpts :: LoggingOpts -> LoggingWithContextT IO a -> IO a
withLoggingOpts opts m = runResourceT $ do
    liftIO $ hSetBuffering stderr LineBuffering
    output <- do
        fileOutput <- case opts.fileLogOpts of
            Nothing -> do
                return $ \_entry -> do
                    return ()
            Just fileLogOpts -> do
                (_key, h) <- allocate (openFile fileLogOpts.dst WriteMode) hClose
                liftIO $ hSetBuffering h LineBuffering
                return $ makeLogEntryPutter fileLogOpts.logOpts h
        let stderrOutput = makeLogEntryPutter opts.stderrLogOpts stderr
        return $ \entry -> do
            fileOutput entry
            stderrOutput entry
    liftIO $ do
        outputMutex <- newMVar ()
        let guardedOutput entry = withMVar outputMutex $ \() -> output entry
        runLoggingWithContextT m guardedOutput

makeLogEntryPutter :: LogOpts -> Handle -> LogEntry -> IO ()
makeLogEntryPutter opts h entry = do
    when (levelAtLeastWithTrace opts.level entry.level) $ do
        hPutBuilder h (formatter entry)
  where
    formatter = logEntryFormatterFor opts.format
