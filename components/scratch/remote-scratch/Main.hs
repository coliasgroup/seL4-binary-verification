{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Redundant $" #-}

module Main
    ( main
    ) where

import qualified Static as T
-- import qualified TCP as T

import BV.Logging

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Debug
import Control.Distributed.Process.Node

import Control.Concurrent (threadDelay)
import Control.Exception.Safe
import Control.Monad (forever, when)
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)
import Control.Monad.Logger (MonadLoggerIO, runStderrLoggingT)
import GHC.Generics (Generic)
import Options.Applicative
import System.Posix.Process (getProcessID)

--

remote :: String -> Process ()
remote s = do
    upid <- liftIO getProcessID
    say $ "remote upid " ++ show upid ++ ": " ++ show s

remotable ['remote]

finalRemoteTable :: RemoteTable
finalRemoteTable = Main.__remoteTable initRemoteTable

--

data Command
  = CommandDriver
  | CommandWorker
  deriving (Generic, Show)

parseOpts :: IO Command
parseOpts = customExecParser
    (prefs (subparserInline <> helpShowGlobals))
    (info (optsParser <**> helper) mempty)

optsParser :: Parser Command
optsParser =
    subparser (mconcat
        [ command "driver"
            (info (pure CommandDriver) mempty)
        , command "worker"
            (info (pure CommandWorker) mempty)
        ])

main :: IO ()
main = do
    cmd <- parseOpts
    runStderrLoggingT $ case cmd of
        CommandDriver -> runDriver
        CommandWorker -> runWorker

driverNodeId :: NodeId
driverNodeId = NodeId T.driverAddr

workerNodeId :: NodeId
workerNodeId = NodeId T.workerAddr

runDriver :: (MonadUnliftIO m, MonadLoggerIO m, MonadThrow m) => m ()
runDriver = do
    run <- askRunInIO
    upid <- liftIO getProcessID
    logInfo $ "driver upid: " ++ show upid
    T.withDriverTransport $ \transport -> do
        node <- liftIO $ newLocalNode transport finalRemoteTable
        let nid = localNodeId node
        logInfo $ "driver node id: " ++ show nid
        when (nid /= driverNodeId) $ do
            throwString "driver unexpected node id"
        liftIO $ runProcess node $ do
            setTraceFlags traceFlags
            startTracer $ \ev -> do
                liftIO $ run $ logDebug $ show ev
            setTraceFlags traceFlags
            pid <- getSelfPid
            liftIO $ run $ logInfo $ "driver process id: " ++ show pid
            remotePid <- spawn workerNodeId ($(mkClosure 'remote) ("foobar" :: String))
            -- remotePid <- spawnLink workerNodeId ($(mkClosure 'remote) ("foobar" :: String))
            liftIO $ run $ logInfo $ "driver spawned remote"
            liftIO $ run $ logInfo $ "remote process id: " ++ show remotePid
            forever $ do
                liftIO $ threadDelay 10000000

runWorker :: (MonadUnliftIO m, MonadLoggerIO m, MonadThrow m) => m ()
runWorker = do
    run <- askRunInIO
    upid <- liftIO getProcessID
    logInfo $ "worker upid: " ++ show upid
    T.withWorkerTransport $ \transport -> do
        node <- liftIO $ newLocalNode transport finalRemoteTable
        let nid = localNodeId node
        logInfo $ "worker node id: " ++ show nid
        when (nid /= workerNodeId) $ do
            throwString "worker unexpected node id"
        liftIO $ runProcess node $ do
            setTraceFlags traceFlags
            startTracer $ \ev -> do
                liftIO $ run $ logDebug $ show ev
            setTraceFlags traceFlags
            pid <- getSelfPid
            liftIO $ run $ logInfo $ "worker process id: " ++ show pid
            forever $ do
                liftIO $ threadDelay 10000000

traceFlags :: TraceFlags
traceFlags = defaultTraceFlags
    { traceSpawned      = Just TraceAll
    , traceDied         = Just TraceAll
    , traceRegistered   = Just TraceAll
    , traceUnregistered = Just TraceAll
    , traceSend         = Just TraceAll
    -- , traceRecv         = Just TraceAll
    , traceNodes        = True
    , traceConnections  = True
    }
