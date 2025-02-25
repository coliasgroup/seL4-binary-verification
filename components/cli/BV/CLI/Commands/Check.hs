module BV.CLI.Commands.Check
    ( runCheck
    ) where

import BV.CLI.Opts
import BV.CLI.SolverList
import BV.CLI.WorkersConfig
import BV.Core
import BV.Logging
import BV.System.Cache.Postgres
import BV.System.Cache.SQLite
import BV.System.Core
import BV.System.Distrib
import BV.System.EvalStages
import BV.System.Local
import BV.System.Utils.Async
import BV.TargetDir

import BV.CLI.Distrib (driverAddr)
import Conduit (awaitForever)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently_)
import Control.Distributed.Process (NodeId (NodeId))
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as BC
import Data.Conduit (connect)
import qualified Data.Conduit.Combinators as C
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Yaml as Y
import Network.Transport (EndPointAddress (EndPointAddress))
import Network.Transport.Static (withStaticTransport)
import Network.Transport.Static.Utils (withDriverPeers)
import Optics
import System.Exit (die, exitFailure)
import System.IO (BufferMode (NoBuffering), Handle, hSetBuffering)
import System.Process (CreateProcess, StdStream (CreatePipe), proc, std_err)
import Text.Printf (printf)

-- TODO ensure --include functions, groups, and checks are actually present

runCheck :: (MonadUnliftIO m, MonadMask m, MonadFail m, MonadLoggerWithContext m) => CheckOpts -> m ()
runCheck opts = do
    let earlyAsmFunctionFilter = getEarlyAsmFunctionFilter opts
    let checkFilter = getCheckFilter opts
    let evalStagesCtx = EvalStagesContext
            { force = True
            , dumpTargetDir = TargetDir <$> opts.dumpTargetDir
            , referenceTargetDir = Just (TargetDir opts.inputTargetDir)
            , mismatchDumpDir = opts.mismatchDir
            }
    input <- liftIO $ readStagesInput earlyAsmFunctionFilter (TargetDir opts.inputTargetDir)
    runChecks <- case opts.workers of
        Nothing -> do
            maxNumConcurrentSolvers <- fromIntegral <$> getMaxNumConcurrentSolvers opts
            let backendConfig = LocalConfig
                    { numJobs = maxNumConcurrentSolvers
                    }
            return $ runLocal backendConfig
        Just workersConfigPath -> do
            workersConfig <- readWorkersConfig workersConfigPath
            return $ \solversConfig checks -> do
                withRunInIO $ \run -> do
                    withDriverPeers (workerCommandsFromWorkersConfig workersConfig) $ \peers stderrs -> do
                        -- TODO ensure all worker stderr is logged in case of driver crash
                        withLinkedAsync (run (handleStderrs stderrs)) $ \_ -> do
                            withStaticTransport driverAddr peers $ \transport -> run $ do
                                let backendConfig = DistribConfig
                                        { transport
                                        , workers = distribWorkerConfigsFromWorkersConfig workersConfig
                                        , stagesInput = input
                                        }
                                runDistrib backendConfig solversConfig checks
    solverList <- readSolverList opts.solvers
    let solversConfig = getSolversConfig opts solverList
    withCacheCtx <- getWithCacheCtx opts
    checks <- filterChecks checkFilter <$> evalStages evalStagesCtx input
    report <- withCacheCtx $ runCacheT $ do
        runChecks solversConfig checks
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

getWithCacheCtx :: (MonadUnliftIO m, MonadLoggerWithContext m) => CheckOpts -> m ((CacheContext m -> m a) -> m a)
getWithCacheCtx opts = do
    augmentCacheContextWithMutualExclusion <- liftIO makeAugmentCacheContextWithMutualExclusion
    let augment with f = with $ f . augmentCacheContextWithLogging . augmentCacheContextWithMutualExclusion
    case map augment $ catMaybes
        [ withSQLiteCacheContext <$> opts.sqliteCache
        , withPostgresCacheContext <$> opts.postgresCache
        ] of
            [] -> return withTrivialCacheContext
            [with] -> return with
            _ -> liftIO $ die "at most one cache may be specified"

solverCommandFromNonEmpty :: NonEmpty String -> SolverCommand
solverCommandFromNonEmpty (path :| args) = SolverCommand { path, args }

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
readSolverList = decodeYamlFile "parsing solver list"

readWorkersConfig :: (MonadIO m, MonadLoggerWithContext m) => FilePath -> m WorkersConfig
readWorkersConfig = decodeYamlFile "parsing workers config"

decodeYamlFile :: (MonadIO m, MonadLoggerWithContext m, FromJSON a) => String -> FilePath -> m a
decodeYamlFile ctx path = do
    r <- liftIO $ Y.decodeFileWithWarnings path
    case r of
        Right (warnings, v) -> do
            withPushLogContext ctx $ do
                for_ warnings $ \warning -> do
                    logWarn (show warning)
            return v
        Left ex -> do
            liftIO $ die (Y.prettyPrintParseException ex)

getEarlyAsmFunctionFilter :: CheckOpts -> AsmFunctionFilter
getEarlyAsmFunctionFilter opts = IncludeExcludeFilter
    { include = Nothing
    , exclude = S.fromList opts.ignoreFunctionsEarly
    }

getCheckFilter :: CheckOpts -> CheckFilter
getCheckFilter opts = CheckFilter
    { pairings =
        let isIncluded = case opts.includeFunctions of
                [] -> const True
                include ->
                    let includeSet = S.fromList include
                    in \pairingId -> pairingId.asm `S.member` includeSet
            isIgnored =
                let ignoreSet = S.fromList opts.ignoreFunctions
                 in \pairingId -> pairingId.asm `S.member` ignoreSet
         in \pairingId -> isIncluded pairingId && not (isIgnored pairingId)
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

handleStderrs :: (MonadUnliftIO m, MonadLoggerWithContext m) => Map EndPointAddress Handle -> m ()
handleStderrs stderrs = withRunInIO $ \run -> do
    forConcurrently_ (M.toList stderrs) $ \(addr, h) -> do
        hSetBuffering h NoBuffering
        run $ C.sourceHandle h `connect` awaitForever (\chunk -> lift $ do
            withPushLogContexts [show addr, "stderr"] $ do
                logWarn $ show chunk)

workerCommandsFromWorkersConfig :: WorkersConfig -> Map EndPointAddress CreateProcess
workerCommandsFromWorkersConfig workersConfig = M.fromList
    [ let addr = EndPointAddress (BC.pack workerName)
          cmd = case workerConfig.command of
                path :| args -> (proc path (args ++ [workerName]))
                    { std_err = CreatePipe
                    }
      in (addr, cmd)
    | (workerName, workerConfig) <- M.toList workersConfig.workers
    ]

distribWorkerConfigsFromWorkersConfig :: WorkersConfig -> Map NodeId DistribWorkerConfig
distribWorkerConfigsFromWorkersConfig workersConfig = M.fromList
    [ let nid = NodeId (EndPointAddress (BC.pack workerName))
          cfg = DistribWorkerConfig
                { numJobs = workerConfig.numJobs
                , priority = workerConfig.priority
                }
        in (nid, cfg)
    | (workerName, workerConfig) <- M.toList workersConfig.workers
    ]
