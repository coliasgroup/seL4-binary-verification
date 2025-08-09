{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.System.Distrib
    ( DistribConfig (..)
    , DistribWorkerConfig (..)
    , __remoteTable
    , runDistrib
    , serverClosureFn
    ) where

import BV.Core.Prelude
import BV.Logging
import BV.SMTLIB2.Command
import BV.System.Core
import BV.System.Utils
import BV.System.Utils.Async
import BV.System.Utils.Distrib
import BV.Utils (expecting, fromIntegerChecked)

import Control.Concurrent (newChan, readChan, writeChan)
import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import Control.Concurrent.STM (modifyTVar', newEmptyTMVarIO, newTVarIO,
                               putTMVar, readTMVar, stateTVar)
import Control.Distributed.Process (NodeId, Process, ProcessId, SendPort,
                                    callLocal, expect, getSelfPid, link,
                                    receiveChan, send, sendChan, spawnLink,
                                    spawnLocal)
import qualified Control.Distributed.Process as D
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Node (LocalNode, runProcess)
import Control.Exception.Safe (MonadMask, SomeException, bracket, throwString,
                               withException)
import Control.Monad (forever, replicateM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO, withRunInIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (lift)
import Data.Binary (Binary)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Traversable (for)
import GHC.Generics (Generic)
import Network.Transport (Transport)
import Optics
import Optics.Passthrough (PermeableOptic (passthrough))
import System.IO (hPutStrLn, stderr)

data DistribConfig
  = DistribConfig
      { transport :: Transport
      , workers :: M.Map NodeId DistribWorkerConfig
      , numEvalCores :: Integer
      , stagesInput :: StagesInput
      }
  deriving (Generic)

data DistribWorkerConfig
  = DistribWorkerConfig
      { numSolverCores :: Integer
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

instance Binary ServerInput

data Request
  = RequestOnline OnlineSolverConfig CheckSubgroupPath
  | RequestOffline OfflineSolverConfig CheckSubgroupPath
  | RequestOfflineSingle OfflineSolverConfig CheckPath
  deriving (Eq, Generic, Ord, Show)

instance Binary Request

data Response
  = ResponseOnline (Either OnlineSolverFailureInfo ())
  | ResponseOffline (Maybe SatResult)
  | ResponseOfflineSingle (Maybe SatResult)
  deriving (Eq, Generic, Ord, Show)

instance Binary Response

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
        run <- mapLoggingWithContextT liftIO askRunInIO
        lift $ forever $ do
            (req, src) <- expect
            callLocal $ do
                link src
                let f (ex :: SomeException) = hPutStrLn stderr $ show ex
                resp <- liftIO $ flip withException f $ run $ executeRequest checks req
                send src resp

serverClosureFn :: (ServerInput, ProcessId) -> Process ()
serverClosureFn (input, replyProcessId) = do
    localLogChan <- liftIO newChan
    let logAction entry = do
            when (levelAtLeastWithTrace LevelDebug entry.level) $ do
                writeChan localLogChan entry
    threadProcessIds <- replicateM (fromIntegerChecked input.numThreads) $ do
        threadProcessId <- spawnLocal $ runLoggingWithContextT (serverThread checks) logAction
        link threadProcessId
        return threadProcessId
    send replyProcessId threadProcessIds
    forever $ do
        entry <- liftIO $ readChan localLogChan
        sendChan input.logChanSend entry
  where
    checks = elaborateChecksFromInput input.stagesInput

remotable ['serverClosureFn]

withBackend :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m) => DistribConfig -> LocalNode -> (SolverBackend m -> m a) -> m a
withBackend config node f = withRunInIO $ \run -> do
    serverThreadProcessIdSlots <- for config.workers $ const newEmptyTMVarIO
    let background = runConcurrently $ ifor_ serverThreadProcessIdSlots $ \workerNodeId slot -> Concurrently $ do
            runProcess node $ do
                selfPid <- getSelfPid
                (logChanSend, logChanRecv) <- D.newChan
                let serverInput = ServerInput
                        { stagesInput = config.stagesInput
                        , logChanSend
                        , numThreads = (config.workers ! workerNodeId).numSolverCores
                        }
                spawnLink workerNodeId ($(mkClosure 'serverClosureFn) (serverInput, selfPid))
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
    let withServerThread :: (ProcessId -> m a) -> m a
        withServerThread f = withRunInIO $ \run -> bracket
            (atomically (stateTVar availableVar takeAvailable))
            (atomically . modifyTVar' availableVar . returnAvailable)
            (\(_prio, pid) -> run (f pid))
    let doReq :: Request -> Prism' Response a -> m a
        doReq req o = withServerThread $ \pid -> withRunInIO $ \run -> runProcess' node $ do
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
    => DistribConfig -> SolversConfig -> Checks -> LocalNode -> m Report
runDistrib config solversConfig checks node = do
    gate <- liftIO $ newSemGate $ sumOf (folded % #numSolverCores) config.workers
    withBackend config node $ \backend -> do
        frontend config.numEvalCores (applySemGate gate) backend solversConfig checks

--

type Priority = Integer

type Available = M.Map Priority [ProcessId]

takeAvailable :: Available -> ((Priority, ProcessId), Available)
takeAvailable av = passthrough (at prio % expecting _Just) f av
  where
    prio:_ = [ prio' | (prio', _:_) <- M.toDescList av ]
    f (pid:pids) = ((prio, pid), pids)

returnAvailable :: (Priority, ProcessId) -> Available -> Available
returnAvailable (prio, pid) av = av & at prio % expecting _Just %~ (++ [pid])
