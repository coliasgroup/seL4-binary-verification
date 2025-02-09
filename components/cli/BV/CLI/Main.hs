module BV.CLI.Main
    ( main
    ) where

import BV.CLI.Commands.Check
import BV.CLI.Commands.ExtractSMT
import BV.CLI.Opts
import BV.Logging

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (allocate, runResourceT)
import Data.ByteString.Builder (hPutBuilder)
import GHC.Conc (getNumProcessors, setNumCapabilities)
import System.IO (Handle, IOMode (WriteMode), hClose, openFile, stderr)
import Text.Pretty.Simple (pPrint)
import Text.Printf (printf)

main :: IO ()
main = do
    opts <- parseOpts
    if opts.justDumpOptions
    then pPrint opts
    else run opts

run :: Opts -> IO ()
run opts = withLoggingOpts opts.globalOpts.loggingOpts $ do
    setNumCapabilitiesAccordingToOpt opts.globalOpts.numCores
    case opts.commandOpts of
        CommandOptsCheck checkOpts -> runCheck checkOpts
        CommandOptsExtractSMT extractSMTOpts -> runExtractSMT extractSMTOpts

setNumCapabilitiesAccordingToOpt :: (MonadIO m, MonadLoggerWithContext m) => Maybe Int -> m ()
setNumCapabilitiesAccordingToOpt opt = do
    numProcs <- liftIO getNumProcessors
    liftIO . setNumCapabilities =<< case opt of
        Just n -> do
            when (n > numProcs) $ do
                logWarn $ printf
                    "--cores option value (%d) exceeds the value returned by getNumProcessors (%d)"
                    n numProcs
            return n
        Nothing -> do
            return $ numProcs - 1

withLoggingOpts :: LoggingOpts -> LoggingWithContextT IO a -> IO a
withLoggingOpts opts m = runResourceT $ do
    output <- do
        fileOutput <- case opts.fileLogOpts of
            Nothing -> do
                return $ \_entry -> do
                    return ()
            Just fileLogOpts -> do
                (_key, h) <- allocate (openFile fileLogOpts.dst WriteMode) hClose
                return $ makeLogEntryPutter fileLogOpts.logOpts h
        let stderrOutput = makeLogEntryPutter opts.stderrLogOpts stderr
        return $ \entry -> do
            fileOutput entry
            stderrOutput entry
    lift $ runLoggingWithContextT m output

makeLogEntryPutter :: LogOpts -> Handle -> LogEntry -> IO ()
makeLogEntryPutter opts h entry = do
    when (levelAtLeastWithTrace opts.level entry.level) $ do
        hPutBuilder h (formatter entry)
  where
    formatter = logEntryFormatterFor opts.format
