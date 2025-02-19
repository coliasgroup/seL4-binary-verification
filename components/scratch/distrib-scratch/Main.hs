{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}

module Main
    ( main
    ) where

import Opts
import Work

import BV.Logging
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_, withAsync)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Debug
import Control.Distributed.Process.Node
import Control.Exception.Safe
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Logger (MonadLoggerIO, runStderrLoggingT)
import Control.Monad.Trans (lift)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Foldable (for_)
import qualified Data.Map as M
import Network.Transport
import Network.Transport.Static
import Network.Transport.Static.Utils
import System.IO (BufferMode (LineBuffering), hSetBuffering)
import System.Posix.Process (getProcessID)
import System.Process (CreateProcess (std_err), StdStream (CreatePipe), proc)

remotable ['remote]

main :: IO ()
main = do
    opts <- parseOpts
    runStderrLoggingT $ case opts of
        CommandDriver opts' -> runDriver opts'
        CommandWorker opts' -> runWorker opts'

driverAddr :: EndPointAddress
driverAddr = EndPointAddress "driver"

driverNodeId :: NodeId
driverNodeId = NodeId driverAddr

workerAddr :: EndPointAddress
workerAddr = EndPointAddress "worker"

workerNodeId :: NodeId
workerNodeId = NodeId workerAddr

runDriver :: (MonadUnliftIO m, MonadLoggerIO m, MonadThrow m) => DriverOpts -> m ()
runDriver opts = do
    upid <- liftIO getProcessID
    logDebug $ "driver upid: " ++ show upid
    let workerCmds = M.singleton workerAddr $ (proc "/proc/self/exe" ["worker"])
            { std_err = CreatePipe
            }
    -- let workerCmds = M.empty
    withRunInIO $ \run -> do
        withDriverPeers workerCmds $ \peers stderrs -> do
            let handleStderrs = forConcurrently_ (M.toList stderrs) $ \(addr, h) -> do
                    hSetBuffering h LineBuffering
                    let go = do
                            bs <- B.hGetSome h 4096
                            unless (B.null bs) $ do
                                let s = C.unpack bs
                                for_ (lines s) $ \line -> do
                                    run $ logDebug $ "stderr from " ++ show addr ++ ": " ++ line
                                go
                    go
            withAsync handleStderrs $ \_ -> do
                -- liftIO $ threadDelay 10000000
                run $ withStaticTransport driverAddr peers $ \transport -> do
                    node <- liftIO $ newLocalNode transport initRemoteTable
                    let nid = localNodeId node
                    run $ logDebug $ "driver node id: " ++ show nid
                    when (nid /= driverNodeId) $ do
                        throwString "driver unexpected node id"
                    liftIO $ runProcess node $ do
                        setTraceFlags traceFlags
                        startTracer $ \ev -> do
                            liftIO $ run $ logDebug $ show ev
                        setTraceFlags traceFlags
                        pid <- getSelfPid
                        liftIO $ run $ logDebug $ "driver process id: " ++ show pid
                        -- remotePid <- spawn workerNodeId ($(mkClosure 'remote) ("fooooobararrrr" :: String))
                        remotePid <- spawnLink workerNodeId ($(mkClosure 'remote) ("fooooobararrrr" :: String))
                        -- remotePid <- spawnLink workerNodeId ($(mkClosure 'remote) ())
                        liftIO $ run $ logDebug $ "driver spawned remote"
                        liftIO $ run $ logDebug $ "remote process id: " ++ show remotePid
                        forever $ do
                            liftIO $ threadDelay 10000000

traceFlags = defaultTraceFlags
  { traceSpawned      = Just TraceAll
  , traceDied         = Just TraceAll
  , traceRegistered   = Just TraceAll
  , traceUnregistered = Just TraceAll
--   , traceSend         = Just TraceAll
--   , traceRecv         = Just TraceAll
  , traceNodes        = True
  , traceConnections  = True
  }

runWorker :: (MonadUnliftIO m, MonadLoggerIO m, MonadThrow m) => WorkerOpts -> m ()
runWorker opts = do
    upid <- liftIO getProcessID
    logDebug $ "worker upid: " ++ show upid
    let peers = workerPeers driverAddr
    withRunInIO $ \run -> do
        run $ withStaticTransport workerAddr peers $ \transport -> do
            node <- newLocalNode transport initRemoteTable
            let nid = localNodeId node
            run $ logDebug $ "worker node id: " ++ show nid
            when (nid /= workerNodeId) $ do
                throwString "worker unexpected node id"
            runProcess node $ do
                setTraceFlags traceFlags
                startTracer $ \ev -> do
                    liftIO $ run $ logDebug $ show ev
                setTraceFlags traceFlags
                pid <- getSelfPid
                liftIO $ run $ logDebug $ "worker process id: " ++ show pid
                forever $ do
                    liftIO $ threadDelay 10000000
