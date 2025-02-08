module BV.CLI.Main
    ( main
    ) where

import BV.CLI.Commands.Check
import BV.CLI.Commands.ExtractSMT
import BV.CLI.Opts
import BV.System.Utils.Logger

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import GHC.Conc (getNumProcessors, setNumCapabilities)
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
    numProcs <- liftIO getNumProcessors
    liftIO . setNumCapabilities =<< case opts.globalOpts.numCores of
        Just n -> do
            when (n > numProcs) $ do
                logWarn $ printf
                    "--cores option value (%d) exceeds the value returned by getNumProcessors (%d)"
                    n numProcs
            return n
        Nothing -> do
            return $ numProcs - 1
    case opts.commandOpts of
        CommandOptsCheck checkOpts -> runCheck opts.globalOpts checkOpts
        CommandOptsExtractSMT extractSMTOpts -> runExtractSMT opts.globalOpts extractSMTOpts

withLoggingOpts :: LoggingOpts -> LoggingWithContextT IO a -> IO a
withLoggingOpts opts m = do
    undefined
    -- withFile logDst WriteMode $ \fileHandle -> do
    --     hSetBuffering fileHandle LineBuffering
    --     let output loc source level str = do
    --             when (filterLevelsBelow LevelInfo source level) $ do
    --                 defaultOutput stderr loc source level str
    --             when (filterLevelsBelow LevelDebug source level) $ do
    --             -- when (filterLevelsBelow levelTrace source level) $ do
    --                 defaultOutput fileHandle loc source level str
    --     flip (runLoggingT . runSimpleLoggingWithContextT) output $
    --         flip runCacheT trivialCacheContext $
    --              run
