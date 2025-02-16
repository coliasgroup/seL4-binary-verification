module BV.CLI.Commands.Check
    ( runCheck
    ) where

import BV.CLI.Opts
import BV.CLI.SolverList
import BV.Core
import BV.Logging
import BV.System.Core
import BV.System.EvalStages
import BV.System.Local
import BV.System.SeL4
import BV.TargetDir

import Control.Concurrent (getNumCapabilities)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Yaml as Y
import Optics
import System.Exit (die, exitFailure)
import Text.Printf (printf)

-- TODO ensure --include functions, groups, and checks are actually present

runCheck :: (MonadUnliftIO m, MonadMask m, MonadFail m, MonadLoggerWithContext m) => CheckOpts -> m ()
runCheck opts = do
    maxNumConcurrentSolvers <- fromIntegral <$> getMaxNumConcurrentSolvers opts
    solverList <- readSolverList opts.solvers
    let solversConfig = getSolversConfig opts solverList
    let checkFilter = getCheckFilter opts
    let evalStagesCtx = EvalStagesContext
            { force = True
            , dumpTargetDir = TargetDir <$> opts.dumpTargetDir
            , referenceTargetDir = Just (TargetDir opts.inputTargetDir)
            , mismatchDumpDir = opts.mismatchDir
            }
    input <- liftIO $ readStagesInput defaultSeL4AsmFunctionFilter (TargetDir opts.inputTargetDir)
    checks <- filterChecks checkFilter <$> evalStages evalStagesCtx input
    let cacheCtx = trivialCacheContext
    let backendConfig = LocalConfig
            { numJobs = maxNumConcurrentSolvers
            , solversConfig
            }
    report <- flip runCacheT cacheCtx $ do
        runLocal backendConfig checks
    let (success, displayedReport) = displayReport report
    liftIO $ putStr displayedReport
    case opts.reportFile of
        Just reportFile -> liftIO $ writeFile reportFile displayedReport
        Nothing -> return ()
    unless success $ do
        liftIO exitFailure

solverCommandFromNonEmpty :: NonEmpty String -> SolverCommand
solverCommandFromNonEmpty (path :| args) = SolverCommand { path, args }

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

getCheckFilter :: CheckOpts -> CheckFilter
getCheckFilter opts = CheckFilter
    { pairings = case opts.includeFunctions of
        [] -> const True
        include ->
            let includeSet = S.fromList include
            in \pairingId -> pairingId.asm `S.member` includeSet
    , groups = case opts.includeGroups of
        [] -> const True
        include -> \groupFingerprint -> or
            [ matchCheckGroupFingerprint pattern groupFingerprint
            | pattern <- include
            ]
    , checks = case opts.includeChecks of
        [] -> const True
        include -> \checkFingerprint -> or
            [ matchCheckFingerprint pattern checkFingerprint
            | pattern <- include
            ]
    }

getSolversConfig :: CheckOpts -> SolverList -> SolversConfig
getSolversConfig opts solverList = SolversConfig
    { online = solverList.online <&> \online -> OnlineSolverConfig
        { command = solverCommandFromNonEmpty online.command
        , modelConfig = ModelConfig
            { memoryMode = online.memoryMode
            }
        , timeout = opts.onlineSolverTimeout
        }
    , offline = OfflineSolversConfig
        { groups =
            [ OfflineSolverGroupConfig
                { commandName
                , command = solverCommandFromNonEmpty g.command
                , scopes = g.scopes
                , modelConfigs = map ModelConfig g.memoryModes
                }
            | (commandName, g) <- M.toAscList solverList.offline
            ]
        , timeout = opts.offlineSolverTimeout
        }
    }

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
