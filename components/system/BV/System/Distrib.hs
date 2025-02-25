{-# LANGUAGE TemplateHaskell #-}

module BV.System.Distrib
    ( DistribConfig (..)
    , DistribWorkerConfig (..)
    , distribRemoteTable
    , runDistrib
    ) where

import BV.Core
import BV.Logging
import BV.System.Core
import BV.System.Utils

import Control.Distributed.Process (NodeId, Process, ProcessId, RemoteTable,
                                    expect, getSelfPid, spawn, spawnLink,
                                    spawnLocal)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node

import BV.Logging (mapLoggingWithContextT)
import BV.System.Utils.Async (withLinkedAsync)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently),
                                 forConcurrently, forConcurrently_, withAsync)
import Control.Concurrent.STM (newEmptyTMVarIO, newTVarIO, putTMVar, readTMVar,
                               readTVar)
import Control.Distributed.Process (link, send)
import Control.Exception.Safe (MonadMask, SomeException, throwIO, try)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO, withRunInIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (lift)
import Data.Binary (Binary)
import Data.ByteString.Builder (hPutBuilder)
import Data.Functor (void)
import Data.List (sortOn)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Traversable (for)
import GHC.Generics (Generic)
import Network.Transport (Transport)
import Optics
import System.IO (hPutStr, stderr)

data DistribConfig
  = DistribConfig
      { transport :: Transport
      , workers :: M.Map NodeId DistribWorkerConfig
      , stagesInput :: StagesInput
      }
  deriving (Generic)

data DistribWorkerConfig
  = DistribWorkerConfig
      { numJobs :: Integer
      , priority :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

data ServerInput
  = ServerInput
      { stagesInput :: StagesInput
      }
  deriving (Eq, Generic, Ord, Show)

instance Binary ServerInput where

data Request
  = RequestOnline OnlineSolverConfig CheckSubgroupPath
  | RequestOffline OfflineSolverConfig CheckSubgroupPath
  | RequestOfflineSingle OfflineSolverConfig CheckPath
  deriving (Eq, Generic, Ord, Show)

instance Binary Request where

executeRequest :: Checks -> Request -> ProcessId -> LoggingWithContextT Process ()
executeRequest checks req src = do
    case req of
        RequestOnline config subgroupId -> do
            result <- mapLoggingWithContextT liftIO $
                localSolverBackend.online config (findCheckSubgroup subgroupId checks)
            lift $ send src result
        RequestOffline config subgroupId -> do
            result <- mapLoggingWithContextT liftIO $
                localSolverBackend.offline config (findCheckSubgroup subgroupId checks)
            lift $ send src result
        RequestOfflineSingle config checkFingerprint -> do
            result <- mapLoggingWithContextT liftIO $
                localSolverBackend.offlineSingle config (findCheck checkFingerprint checks)
            lift $ send src result

server :: ServerInput -> Process ()
server input = do
    let logAction entry = do
            when (levelAtLeastWithTrace LevelDebug entry.level) $ do
                hPutBuilder stderr (formatLogEntryHuman entry)
    forever $ do
        (req, src) <- expect
        spawnLocal (runLoggingWithContextT (executeRequest checks req src) logAction)
        -- TODO >>= link
        return ()
  where
    checks = elaborateChecksFromInput input.stagesInput

remotable ['server]

distribRemoteTable :: RemoteTable
distribRemoteTable = __remoteTable initRemoteTable

withBackend :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m) => DistribConfig -> (SolverBackend m -> m a) -> m a
withBackend config f = do
    node <- liftIO $ newLocalNode config.transport distribRemoteTable
    serverProcessIdSlots <- for config.workers $ \_ -> liftIO newEmptyTMVarIO
    let background = runConcurrently $ ifor_ serverProcessIdSlots $ \workerNodeId slot -> Concurrently $ do
            runProcess node $ do
                let serverInput = ServerInput
                        { stagesInput = config.stagesInput
                        }
                serverProcessId <- spawnLink workerNodeId ($(mkClosure 'server) serverInput)
                liftIO $ atomically $ do
                    putTMVar slot serverProcessId
                liftIO $ forever $ threadDelay maxBound
    withRunInIO $ \run -> do
        withLinkedAsync background $ \_ -> run $ do
            serverProcessIds <- for serverProcessIdSlots $ liftIO . atomically . readTMVar
            let serverProcesses = M.fromList
                    [ (serverProcessId, config.workers ! workerNodeId)
                    | (workerNodeId, serverProcessId) <- M.toList serverProcessIds
                    ]
            backend <- solverBackendFromServerProcesses node serverProcesses
            f backend
                -- return (serverProcessId, workerConfig)
    -- serverProcesses <- fmap M.fromList $ liftIO $ forConcurrently (M.toList config.workers) $ \(workerNodeId, workerConfig) -> do
    --     runProcess' node $ do
    --         let serverInput = ServerInput
    --                 { stagesInput = config.stagesInput
    --                 }
    --         serverProcessId <- spawn workerNodeId ($(mkClosure 'server) serverInput)
    --         return (serverProcessId, workerConfig)


runProcess' :: LocalNode -> Process a -> IO a
runProcess' node proc = do
  done <- newEmptyMVar
  void $ forkProcess node $ try proc >>= liftIO . putMVar done
  takeMVar done >>= either (throwIO :: SomeException -> IO a) return

solverBackendFromServerProcesses :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m) => LocalNode -> M.Map ProcessId DistribWorkerConfig -> m (SolverBackend m)
solverBackendFromServerProcesses node serverProcesses = do
    availableVar <- liftIO $ newTVarIO $ map snd $ sortOn fst
        [ (config.priority, (serverProcessId, config.numJobs))
        | (serverProcessId, config) <- M.toList serverProcesses
        ]
    let withServerJob f = do
            pid <- liftIO $ atomically $ do
                available <- readTVar availableVar
                return $ fst $ head available
            f pid
    return $ SolverBackend
        { online = \config subgroup -> withServerJob $ \serverProcessId -> withRunInIO $ \run -> runProcess' node $ do
            src <- getSelfPid
            let req = RequestOnline config (pathForCheckSubgroup subgroup)
            send serverProcessId (req, src)
            expect
        , offline = \config subgroup -> withServerJob $ \serverProcessId -> withRunInIO $ \run -> runProcess' node $ do
            src <- getSelfPid
            let req = RequestOffline config (pathForCheckSubgroup subgroup)
            send serverProcessId (req, src)
            expect
        , offlineSingle = \config check -> withServerJob $ \serverProcessId -> withRunInIO $ \run -> runProcess' node $ do
            src <- getSelfPid
            let req = RequestOfflineSingle config (pathForCheck check)
            send serverProcessId (req, src)
            expect
        }

runDistrib
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => DistribConfig -> SolversConfig -> Checks -> m Report
runDistrib config solversConfig checks = do
    gate <- liftIO $ newSemGate $ sumOf (folded % #numJobs) config.workers
    withBackend config $ \backend -> do
        frontend (applySemGate gate) backend solversConfig checks
