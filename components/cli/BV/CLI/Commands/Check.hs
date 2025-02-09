module BV.CLI.Commands.Check
    ( runCheck
    ) where

import BV.CLI.Opts
import BV.CLI.SolverList
import BV.Core.ExecuteSMTProofChecks
import BV.Core.Types
import BV.Logging
import BV.System.Backend.Core
import BV.System.Backend.Local
import BV.System.Cache
import BV.System.EvalStages
import BV.System.Frontend
import BV.System.SeL4
import BV.System.SolversConfig
import BV.System.WithFingerprints
import BV.TargetDir

import Control.Concurrent (getNumCapabilities)
import Control.Monad (when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Foldable (for_)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Yaml as Y
import Optics
import System.Exit (die)
import Text.Printf (printf)

runCheck :: (MonadUnliftIO m, MonadMask m, MonadFail m, MonadLoggerWithContext m) => CheckOpts -> m ()
runCheck opts = do
    maxNumConcurrentSolvers <- fromIntegral <$> getMaxNumConcurrentSolvers opts
    solverList <- readSolverList opts.solvers
    let solversConfig = SolversConfig
            { online = solverList.online <&> \online -> OnlineSolverConfig
                { command = online.command
                , modelConfig = ModelConfig
                    { memoryMode = online.memoryMode
                    }
                , timeout = opts.onlineSolverTimeout
                }
            , offline = OfflineSolversConfig
                { groups =
                    [ OfflineSolverGroupConfig
                        { commandName
                        , command = g.command
                        , scopes = g.scopes
                        , modelConfigs = map ModelConfig g.memoryModes
                        }
                    | (commandName, g) <- M.toAscList solverList.offline
                    ]
                , timeout = opts.offlineSolverTimeout
                }
            }
    input <- liftIO $ readStagesInput defaultSeL4AsmFunctionFilter (TargetDir opts.inputTargetDir)
    let evalStagesCtx = EvalStagesContext
            { force = True
            , dumpTargetDir = TargetDir <$> opts.dumpTargetDir
            , referenceTargetDir = Just (TargetDir opts.inputTargetDir)
            , mismatchDumpDir = opts.mismatchDir
            }
    unfilteredChecks <- evalStages evalStagesCtx input
    let checks = case opts.includeFunctions of
            [] -> unfilteredChecks
            include ->
                let includeSet = S.fromList include
                 in unfilteredChecks & #unwrap %~ M.filterWithKey (\pairingId _ -> pairingId.asm `S.member` includeSet)
    -- TODO
    let cacheCtx = trivialCacheContext
    report <- flip runCacheT cacheCtx $ do
        let backendConfig = LocalBackendConfig
                { numJobs = maxNumConcurrentSolvers
                , backendCoreConfig = BackendCoreConfig
                    { solversConfig
                    }
                }
        localBackend backendConfig (adornWithFingerprints checks)
    let displayedReport = displayReport report
    liftIO $ putStr displayedReport
    case opts.reportFile of
        Just reportFile -> liftIO $ writeFile reportFile displayedReport
        Nothing -> return ()
    return ()

getMaxNumConcurrentSolvers :: (MonadIO m, MonadLogger m) => CheckOpts -> m Int
getMaxNumConcurrentSolvers opts = do
    numCaps <- liftIO getNumCapabilities
    case opts.maxNumConcurrentSolvers of
        Just n -> do
            when (n > numCaps) $ do
                logWarn $ printf
                    "--jobs option value (%d) exceeds the value returned by getNumCapabilities (%d)"
                    n numCaps
            return n
        Nothing -> do
            return numCaps

readSolverList :: (MonadIO m, MonadLoggerWithContext m) => FilePath -> m SolverList
readSolverList path = do
    r <- liftIO $ Y.decodeFileWithWarnings path
    case r of
        Right (warnings, v) -> do
            withPushLogContext "parsing solver list" $ do
                for_ warnings $ \warning -> do
                    logWarn (show warning)
            return v
        Left ex -> do
            liftIO $ die (Y.prettyPrintParseException ex)
