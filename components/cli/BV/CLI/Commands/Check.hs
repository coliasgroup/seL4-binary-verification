module BV.CLI.Commands.Check
    ( runCheck
    ) where

import BV.CLI.Opts
import BV.CLI.SolverList
import BV.Core
import BV.Logging
import BV.System.Backend.Core
import BV.System.Backend.Local
import BV.System.Core.Cache
import BV.System.Core.Fingerprinting
import BV.System.Core.WithFingerprints
import BV.System.EvalStages
import BV.System.Frontend
import BV.System.SeL4
import BV.System.SolversConfig
import BV.TargetDir

import Control.Concurrent (getNumCapabilities)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Foldable (for_)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Yaml as Y
import Optics
import System.Exit (die, exitFailure)
import Text.Printf (printf)

-- TODO ensure --include functions, groups, and checks are actually present

runCheck :: (MonadUnliftIO m, MonadMask m, MonadFail m, MonadLoggerWithContext m) => CheckOpts -> m ()
runCheck opts = do
    -- TODO do this check elsewhere
    -- when (length (filter (> 0) [length opts.includeGroups, length opts.includeChecks]) > 1) $ do
    --     liftIO $
    --         die "--include-functions, --include-groups, and --inlude-checks are mutually exclusive"
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
    let filterPairings = case opts.includeFunctions of
            [] -> id
            include ->
                let includeSet = S.fromList include
                 in #unwrap %~ M.filterWithKey (\pairingId _ -> pairingId.asm `S.member` includeSet)
    let filterGroups = case opts.includeGroups of
            [] -> id
            include ->
                #unwrap % traversed %~
                    filter (\group -> any (\p -> matchSMTProofCheckGroupFingerprint p group.fingerprint) include)
    checks <- filterGroups . filterPairings . decorateWithFingerprints <$>
        evalStages evalStagesCtx input
    -- TODO
    let cacheCtx = trivialCacheContext
    let backendConfig = LocalBackendConfig
            { numJobs = maxNumConcurrentSolvers
            , backendCoreConfig = BackendCoreConfig
                { solversConfig
                }
            }
    report <- flip runCacheT cacheCtx $ do
        case opts.includeChecks  of
            [] -> localBackend backendConfig checks
            include ->
                let f check = any (\p -> matchSMTProofCheckFingerprint p check.imp.meta.fingerprint) include
                    justTheseChecks = M.map (concatMap (filter f . ungroupSMTProofCheckGroup . (.inner))) checks.unwrap
                 in localBackendJustTheseChecks backendConfig justTheseChecks
    let (success, displayedReport) = displayReport report
    liftIO $ putStr displayedReport
    case opts.reportFile of
        Just reportFile -> liftIO $ writeFile reportFile displayedReport
        Nothing -> return ()
    unless success $ do
        liftIO exitFailure

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
