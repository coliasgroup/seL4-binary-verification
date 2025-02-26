{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.System.Distrib
    ( DistribConfig (..)
    , DistribWorkerConfig (..)
    , distribRemoteTable
    , runDistrib
    ) where

import BV.Core
import BV.Logging
import BV.SMTLIB2.Command
import BV.System.Core
import BV.System.Utils
import BV.System.Utils.Async

import Control.Concurrent (newChan, newEmptyMVar, putMVar, readChan, takeMVar,
                           writeChan)
import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import Control.Concurrent.STM (modifyTVar', newEmptyTMVarIO, newTVarIO,
                               putTMVar, readTMVar, stateTVar)
import Control.Distributed.Process (NodeId, Process, ProcessId, RemoteTable,
                                    SendPort, expect, getSelfPid, link,
                                    receiveChan, send, sendChan, spawnLink,
                                    spawnLocal)
import qualified Control.Distributed.Process as D
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Node (LocalNode, forkProcess,
                                         initRemoteTable, newLocalNode,
                                         runProcess)
import Control.Exception.Safe (MonadMask, SomeException, throwIO, throwString,
                               try)
import Control.Monad (forever, replicateM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (lift)
import Data.Binary (Binary)
import Data.Functor (void)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Traversable (for)
import GHC.Generics (Generic)
import Network.Transport (Transport)
import Optics
import Optics.Passthrough (PermeableOptic (passthrough))
import Text.Pretty.Simple (pPrint)

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
      , logChanSend :: SendPort LogEntry
      , numThreads :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

instance Binary ServerInput where

data Request
  = RequestOnline OnlineSolverConfig CheckSubgroupPath
  | RequestOffline OfflineSolverConfig CheckSubgroupPath
  | RequestOfflineSingle OfflineSolverConfig CheckPath
  deriving (Eq, Generic, Ord, Show)

instance Binary Request where

data Response
  = ResponseOnline (Either OnlineSolverFailureInfo ())
  | ResponseOffline (Maybe SatResult)
  | ResponseOfflineSingle (Maybe SatResult)
  deriving (Eq, Generic, Ord, Show)

instance Binary Response where

executeRequest :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m) => Checks -> Request -> m Response
executeRequest checks = \case
    RequestOnline config subgroupId ->
        ResponseOnline <$>
            localSolverBackend.online config (findCheckSubgroup subgroupId checks)
    RequestOffline config subgroupId ->
        ResponseOffline <$>
            localSolverBackend.offline config (findCheckSubgroup subgroupId checks)
    RequestOfflineSingle config checkFingerprint ->
        ResponseOfflineSingle <$>
            localSolverBackend.offlineSingle config (findCheck checkFingerprint checks)

serverThread :: Checks -> LoggingWithContextT Process ()
serverThread checks = do
    selfPid <- lift getSelfPid
    withPushLogContext (show selfPid) $ do
        forever $ do
            (req, src) <- lift expect
            resp <- mapLoggingWithContextT liftIO $ executeRequest checks req
            lift $ send src resp

server :: (ServerInput, ProcessId) -> Process ()
server (input, replyProcessId) = do
    localLogChan <- liftIO newChan
    let logAction entry = do
            when (levelAtLeastWithTrace LevelDebug entry.level) $ do
                writeChan localLogChan entry
    threadProcessIds <- replicateM (fromInteger input.numThreads) $ do
        threadProcessId <- spawnLocal $ runLoggingWithContextT (serverThread checks) logAction
        link threadProcessId
        return threadProcessId
    send replyProcessId threadProcessIds
    forever $ do
        entry <- liftIO $ readChan localLogChan
        sendChan input.logChanSend entry
  where
    checks = elaborateChecksFromInput input.stagesInput

remotable ['server]

distribRemoteTable :: RemoteTable
distribRemoteTable = __remoteTable initRemoteTable

withBackend :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m) => DistribConfig -> (SolverBackend m -> m a) -> m a
withBackend config f = withRunInIO $ \run -> do
    node <- newLocalNode config.transport distribRemoteTable
    serverThreadProcessIdSlots <- for config.workers $ const newEmptyTMVarIO
    let background = runConcurrently $ ifor_ serverThreadProcessIdSlots $ \workerNodeId slot -> Concurrently $ do
            runProcess node $ do
                selfPid <- getSelfPid
                (logChanSend, logChanRecv) <- D.newChan
                let serverInput = ServerInput
                        { stagesInput = config.stagesInput
                        , logChanSend
                        , numThreads = (config.workers ! workerNodeId).numJobs
                        }
                spawnLink workerNodeId ($(mkClosure 'server) (serverInput, selfPid))
                serverThreadProcessIds <- expect
                liftIO $ atomically $ do
                    putTMVar slot serverThreadProcessIds
                forever $ do
                    entry <- receiveChan logChanRecv
                    liftIO $ run $ logEntryWithContext entry
    withLinkedAsync background $ \_ -> run $ do
        serverThreadProcessIds <- for serverThreadProcessIdSlots $ liftIO . atomically . readTMVar
        let available = M.fromListWith (<>)
                [ ((config.workers ! workerNodeId).priority, jobPids)
                | (workerNodeId, jobPids) <- M.toList serverThreadProcessIds
                ]
        backend <- solverBackendFromServerProcesses node available
        f backend

solverBackendFromServerProcesses :: forall m. (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m) => LocalNode -> Available -> m (SolverBackend m)
solverBackendFromServerProcesses node availableInit = do
    availableVar <- liftIO $ newTVarIO availableInit
    let withServerThread f = do
            (prio, pid) <- liftIO $ atomically $ do
                stateTVar availableVar takeAvailable
            r <- f pid
            liftIO $ atomically $ do
                modifyTVar' availableVar $ returnAvailable prio pid
            return r
    let doReq :: Request -> Prism' Response a -> m a
        doReq req o = withServerThread $ \pid -> withRunInIO $ \run -> runProcessForOutput node $ do
            src <- getSelfPid
            liftIO $ run $ logDebug $ "making request to " ++ show pid
            send pid (req, src)
            resp <- expect
            case preview o resp of
                Just x -> return x
                Nothing -> throwString "unexpected response"
    return $ SolverBackend
        { online = \config subgroup ->
            doReq (RequestOnline config (pathForCheckSubgroup subgroup)) #_ResponseOnline
        , offline = \config subgroup ->
            doReq (RequestOffline config (pathForCheckSubgroup subgroup)) #_ResponseOffline
        , offlineSingle = \config check ->
            doReq (RequestOfflineSingle config (pathForCheck check)) #_ResponseOfflineSingle
        }

runDistrib
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => DistribConfig -> SolversConfig -> Checks -> m Report
runDistrib config solversConfig checks = do
    gate <- liftIO $ newSemGate $ sumOf (folded % #numJobs) config.workers
    withBackend config $ \backend -> do
        frontend (applySemGate gate) backend solversConfig checks

--

runProcessForOutput :: LocalNode -> Process a -> IO a
runProcessForOutput node proc = do
  done <- newEmptyMVar
  void $ forkProcess node $ try proc >>= liftIO . putMVar done
  takeMVar done >>= either (throwIO :: SomeException -> IO a) return

--

type Priority = Integer

type Available = M.Map Priority [ProcessId]

takeAvailable :: Available -> ((Priority, ProcessId), Available)
takeAvailable av = passthrough (at prio % expecting _Just) f av
  where
    prio:_ = [ prio' | (prio', _:_) <- M.toList av ]
    f (pid:pids) = ((prio, pid), pids)

returnAvailable :: Priority -> ProcessId -> Available -> Available
returnAvailable prio pid av = av & at prio % expecting _Just %~ (++ [pid])
