module BV.CLI.Main
    ( main
    ) where

import BV.CLI.Commands.Check
import BV.CLI.Commands.ExtractSMT
import BV.CLI.Commands.FormatSMT
import BV.CLI.Commands.Worker
import BV.CLI.Opts
import BV.Logging

import Control.Concurrent (newMVar, withMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (allocate, runResourceT)
import Data.ByteString.Builder (hPutBuilder)
import Data.Foldable (traverse_)
import GHC.Conc (setNumCapabilities)
import System.IO (BufferMode (LineBuffering), Handle, IOMode (WriteMode),
                  hClose, hSetBuffering, openFile, stderr)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
    opts <- parseOpts
    if opts.justDumpOptions
    then pPrint opts
    else run opts

run :: Opts -> IO ()
run opts = do
    setNumCapabilitiesAccordingToOpt opts.globalOpts.numCores
    case opts.commandOpts of
        CommandOptsWorker opts' -> runWorker opts'
        CommandOptsNotWorker notWorkerGlobalOpts notWorkerCommandOpts -> runNotWorker notWorkerGlobalOpts notWorkerCommandOpts

runNotWorker :: NotWorkerGlobalOpts -> NotWorkerCommandOpts -> IO ()
runNotWorker notWorkerGlobalOpts notWorkerCommandOpts = do
    withLoggingOpts notWorkerGlobalOpts.loggingOpts $ do
        case notWorkerCommandOpts of
            CommandOptsCheck opts -> runCheck opts
            CommandOptsExtractSMT opts -> runExtractSMT opts
            CommandOptsFormatSMT opts -> runFormatSMT opts

setNumCapabilitiesAccordingToOpt :: Maybe Int -> IO ()
setNumCapabilitiesAccordingToOpt = traverse_ setNumCapabilities

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
