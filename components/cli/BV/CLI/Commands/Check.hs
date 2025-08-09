{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.CLI.Commands.Check
    ( runCheck
    ) where

import BV.CLI.Distrib
import BV.CLI.Opts
import BV.CLI.SolverList
import BV.CLI.WorkersConfig
import BV.Core.Prelude
import BV.Logging
import BV.System.Cache.Postgres
import BV.System.Cache.SQLite
import BV.System.Core
import BV.System.Distrib
import BV.System.EvalStages
import BV.System.Local
import BV.System.Utils.Async
import BV.System.Utils.Distrib (runProcess')
import BV.TargetDir
import BV.Utils (ensure, fromIntegerChecked)

import Conduit (awaitForever)
import Control.Applicative (Alternative ((<|>)), asum)
import Control.Concurrent.Async (Concurrently (Concurrently), forConcurrently_,
                                 runConcurrently)
import Control.Distributed.Process (NodeId (NodeId), call)
import Control.Distributed.Process.Closure (functionTDict, mkClosure)
import Control.Distributed.Process.Node (LocalNode, newLocalNode)
import Control.Exception.Safe (throwString)
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
import Data.Function (applyWhen)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import qualified Data.Set as S
import qualified Data.Yaml as Y
import GHC.Conc (getNumProcessors, setNumCapabilities)
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
    let cFunctionPrefix = opts.cFunctionPrefix
    let rodataInputRanges = getRODataInputRanges opts
    let checkFilter = getCheckFilter opts
    let evalStagesCtx = EvalStagesContext
            { forceAll = opts.forceEvalStages
            , dumpTargetDir = TargetDir <$> opts.dumpTargetDir
            , referenceTargetDir = TargetDir <$> opts.referenceTargetDir
            , mismatchDumpDir = opts.mismatchDir
            }
    input <- liftIO $ readStagesInput
        earlyAsmFunctionFilter
        cFunctionPrefix
        rodataInputRanges
        (TargetDir opts.inputTargetDir)
    let evalChecks = filterChecks checkFilter <$> evalStages evalStagesCtx input
    if opts.justCompareChecks
    then do
        evalChecks
        liftIO $ putStrLn "Success"
    else do
        solverList <- readSolverList opts.solvers
        let solversConfig = getSolversConfig opts solverList
        withCacheCtx <- getWithCacheCtx opts
        runChecks <- case opts.workers of
            Nothing -> do
                (numEvalCores, numSolverCores) <- configureLocalCPUResources opts.numCoresOpts
                checkNumSolverCores solversConfig numSolverCores
                let backendConfig = LocalConfig
                        { numEvalCores
                        , numSolverCores
                        }
                return $ runLocal backendConfig solversConfig
            Just workersConfigPath -> do
                workersConfig <- readWorkersConfig workersConfigPath
                return $ \checks -> do
                    withRunInIO $ \run -> do
                        withDriverPeers (workerCommandsFromWorkersConfig workersConfig) $ \peers stderrs -> do
                            -- TODO ensure all worker stderr is logged in case of driver crash with some kind of flush on exception
                            withLinkedAsync (run (handleStderrs stderrs)) $ \_ -> do
                                withStaticTransport driverAddr peers $ \transport -> run $ do
                                    localNode <- liftIO $ newLocalNode transport distribRemoteTable
                                    let nids = map nodeIdFromWorkerName $ M.keys workersConfig.workers
                                    withWatchdogs localNode nids $ do
                                        numEvalCores <- configureDriverCPUResources opts.numCoresOpts
                                        numSolverCoresByWorker <- ifor workersConfig.workers $ \workerName workerConfig ->
                                            configureWorkerCPUResources
                                                localNode
                                                (nodeIdFromWorkerName workerName)
                                                workerConfig.numSolverCores
                                        checkNumSolverCores solversConfig $ sumOf folded numSolverCoresByWorker
                                        let backendConfig = DistribConfig
                                                { transport
                                                , workers = M.fromList $ flip map (M.toList workersConfig.workers) $ \(workerName, workerConfig) ->
                                                    (,) (nodeIdFromWorkerName workerName) $ DistribWorkerConfig
                                                        { numSolverCores = numSolverCoresByWorker M.! workerName
                                                        , priority = workerConfig.priority
                                                        }
                                                , numEvalCores
                                                , stagesInput = input
                                                }
                                        runDistrib backendConfig solversConfig checks localNode
        checks <- evalChecks
        report <- withCacheCtx $ runCacheT $ runChecks checks
        let (success, displayedReport) = displayReport report
        liftIO $ putStr displayedReport
        case opts.reportFile of
            Just reportFile -> liftIO $ writeFile reportFile displayedReport
            Nothing -> return ()
        unless success $ do
            liftIO exitFailure

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

makeIncludExcludeFilter :: [Ident] -> [Ident] -> IncludeExcludeFilter Ident
makeIncludExcludeFilter include exclude = IncludeExcludeFilter
    { include = case include of
        [] -> Nothing
        xs -> Just $ S.fromList xs
    , exclude = S.fromList exclude
    }

getEarlyAsmFunctionFilter :: CheckOpts -> AsmFunctionFilter
getEarlyAsmFunctionFilter opts = makeIncludExcludeFilter
    opts.includeFunctionsEarly
    opts.ignoreFunctionsEarly

getRODataInputRanges :: CheckOpts -> RODataInputRanges
getRODataInputRanges opts =
    map (RODataInputRangeTypeSection,) opts.rodataSections
        ++ map (RODataInputRangeTypeSymbol,) opts.rodataSymbols

getCheckFilter :: CheckOpts -> CheckFilter
getCheckFilter opts = CheckFilter
    { pairings = \pairingId -> applyIncludeExcludeFilter pairingsFilter (getAsm pairingId)
    , groups = \pairingId -> M.lookup (getAsm pairingId) groupPatterns <&> \pats ->
        \groupFingerprint -> or
            [ matchCheckGroupFingerprint pat groupFingerprint
            | pat <- pats
            ]
    , checks = \pairingId -> M.lookup (getAsm pairingId) checkPatterns <&> \pats ->
        \checkFingerprint -> or
            [ matchCheckFingerprint pat checkFingerprint
            | pat <- pats
            ]
    }
  where
    pairingsFilter =
        makeIncludExcludeFilter
            (opts.includeFunctions ++ map fst opts.includeGroups ++ map fst opts.includeChecks)
            opts.ignoreFunctions
    groupPatterns = M.fromListWith (<>) (map (over _2 (:[])) opts.includeGroups)
    checkPatterns = M.fromListWith (<>) (map (over _2 (:[])) opts.includeChecks)

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
                path :| args -> (proc path (args ++ ["worker", workerName]))
                    { std_err = CreatePipe
                    }
      in (addr, cmd)
    | (workerName, workerConfig) <- M.toList workersConfig.workers
    ]

nodeIdFromWorkerName :: WorkerName -> NodeId
nodeIdFromWorkerName workerName = NodeId $ EndPointAddress $ BC.pack workerName

withWatchdogs :: MonadUnliftIO m => LocalNode -> [NodeId] -> m a -> m a
withWatchdogs localNode nids m = withRunInIO $ \run -> do
    let watchdogs = asum $ flip map nids $ \nid -> Concurrently $ do
            cond <- runProcess' localNode $ watchdog nid
            throwString $ show cond
    liftIO $ runConcurrently $ watchdogs <|> Concurrently (run m)

configureLocalCPUResources
    :: (MonadLoggerWithContext m, MonadIO m)
    => CheckNumCoresOpts -> m (Integer, Integer)
configureLocalCPUResources opts = do
    numProcs <- toInteger <$> liftIO getNumProcessors
    let minNumCores = fromMaybe 1 opts.numEvalCores + fromMaybe 1 opts.numSolverCores
    let numCoresOpt = (+) <$> opts.numEvalCores <*> opts.numSolverCores
    let (numCaps, numCores) = computeCapsAndCores numProcs minNumCores numCoresOpt
    let (numEvalCores, numSolverCores) = case (opts.numEvalCores, opts.numSolverCores) of
            (Just n, _) -> (n, numCores - n)
            (_, Just n) -> (numCores - n, n)
            (Nothing, Nothing) ->
                let extra = numCores - 2
                    eval = 1 + (extra `div` 3)
                 in (eval, numCores - eval)
    liftIO $ setNumCapabilities $ fromIntegerChecked numCaps
    logInfo $ printf "local numCaps=%d numEvalCores=%d numSolverCores=%d"
        numCaps numEvalCores numSolverCores
    return (numEvalCores, numSolverCores)

configureDriverCPUResources
    :: (MonadLoggerWithContext m, MonadIO m)
    => CheckNumCoresOpts -> m Integer
configureDriverCPUResources opts = do
    numProcs <- toInteger <$> liftIO getNumProcessors
    unless (isNothing opts.numSolverCores) $ do
        logWarn "driver ignoring --num-solver-cores"
    let (numCaps, numEvalCores) = computeCapsAndCores numProcs 1 opts.numEvalCores
    liftIO $ setNumCapabilities $ fromIntegerChecked numCaps
    logInfo $ printf "driver numCaps=%d numEvalCores=%d" numCaps numEvalCores
    return numEvalCores

configureWorkerCPUResources
    :: (MonadLoggerWithContext m, MonadIO m)
    => LocalNode -> NodeId -> Maybe Integer -> m Integer
configureWorkerCPUResources localNode nid numSolverCoresOpt = do
    numProcs <- fmap toInteger $ runProcess' localNode $ call
        $(functionTDict 'getNumProcessorsClosureFn)
        nid
        ($(mkClosure 'getNumProcessorsClosureFn)
            ())
    let (numCaps, numSolverCores) = computeCapsAndCores numProcs 1 numSolverCoresOpt
    runProcess' localNode $ call
        $(functionTDict 'setNumCapabilitiesClosureFn)
        nid
        ($(mkClosure 'setNumCapabilitiesClosureFn)
            (fromIntegerChecked numCaps :: Int))
    logInfo $ printf "worker %s numCaps=%d numSolverCores=%d" (show nid) numCaps numSolverCores
    return numSolverCores

computeCapsAndCores :: Integer -> Integer -> Maybe Integer -> (Integer, Integer)
computeCapsAndCores numProcs minNumCores numCoresOpt = ensure check (numCaps, numCores)
  where
    maxCaps = case numProcs of
        1 -> 1
        n | n > 1 -> n - 1
    minCaps = case numCoresOpt of
        Nothing -> minNumCores
        Just n | n >= 1 -> n
    requestedCaps = case numCoresOpt of
        Nothing -> maxCaps
        Just n -> n + 1
    numCaps = restrict (minCaps, maxCaps) requestedCaps
    numCores = case numCoresOpt of
        Nothing -> max minNumCores $ applyWhen (numCaps >= 3) (subtract 1) numCaps
        Just n -> n
    check = minNumCores <= numCores && numCores <= numCaps

restrict :: (Integer, Integer) -> Integer -> Integer
restrict (lo, hi) n = case (lo <= n, n <= hi) of
    (False, _) -> lo
    (_, False) -> hi
    _ -> n

checkNumSolverCores :: (MonadIO m, MonadLogger m) => SolversConfig -> Integer -> m ()
checkNumSolverCores solversConfig n = do
    let min_ = minNumJobs solversConfig
    when (n < min_) $ do
        liftIO $ die $ printf
            "--jobs option value (%d) is less than the minimum required by the provided solver config (%d)"
            n min_
