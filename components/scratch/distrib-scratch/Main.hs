{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
    ( main
    ) where

import Work
import Opts

import Control.Distributed.Process
import qualified Control.Distributed.Process.Backend.SimpleLocalnet as S
import Control.Distributed.Process.Node
import Control.Exception.Safe
import Control.Distributed.Process.Closure
import Network.Socket
import System.Posix.Process (getProcessID)
import Network.Transport
import qualified Network.Transport as NT (Transport)
import qualified Network.Transport.TCP as NT (TCPAddr (Addressable),
                                              TCPAddrInfo (TCPAddrInfo),
                                              createTransport,
                                              defaultTCPParameters)
import Control.Monad (forever, when)
import Control.Concurrent (threadDelay)

remotable ['remote]

main :: IO ()
main = do
    opts <- parseOpts
    case opts of
        CommandDriver opts' -> runDriver opts'
        CommandWorker opts' -> runWorker opts'

mkTransport :: HostName -> ServiceName -> IO Transport
mkTransport host port = do
    r <- NT.createTransport (NT.Addressable (NT.TCPAddrInfo host port (host,))) NT.defaultTCPParameters
    case r of
        Left err -> throwString $ show err
        Right t -> return t

driverNodeId :: NodeId
driverNodeId = NodeId (EndPointAddress "localhost:9001:0")

workerNodeId :: NodeId
workerNodeId = NodeId (EndPointAddress "localhost:9002:0")

workerNodeIdBad :: NodeId
workerNodeIdBad = NodeId (EndPointAddress "xlocalhost:9002:0")

runDriver :: DriverOpts -> IO ()
runDriver opts = do
    upid <- getProcessID
    putStrLn $ "driver pupidid: " ++ show upid
    let host = "localhost"
    let port = "9001"
    trans <- mkTransport host port
    node <- newLocalNode trans initRemoteTable
    let nid = localNodeId node
    putStrLn $ "driver node id: " ++ show nid
    when (nid /= driverNodeId) $ do
        throwString $ "driver unexpected node id: " ++ show nid
    runProcess node $ do
        pid <- getSelfPid
        say $ "driver process id: " ++ show pid
        -- remotePid <- spawn workerNodeId ($(mkClosure 'remote) ())
        remotePid <- spawnLink workerNodeId ($(mkClosure 'remote) ())
        -- remotePid <- spawn workerNodeIdBad ($(mkClosure 'remote) ())
        -- remotePid <- spawn workerNodeId ($(mkClosure 'remote) (1337 :: Int))
        say $ "driver spawned remote"
        say $ "remote process id: " ++ show remotePid
        forever $ do
            liftIO $ threadDelay 10000000
            -- msg :: String <- expect
            -- liftIO $ putStrLn $ "driver msg: " ++ show msg

runWorker :: WorkerOpts -> IO ()
runWorker opts = do
    upid <- getProcessID
    putStrLn $ "worker upid: " ++ show upid
    let host = "localhost"
    let port = "9002"
    trans <- mkTransport host port
    node <- newLocalNode trans initRemoteTable
    let nid = localNodeId node
    putStrLn $ "worker node id: " ++ show nid.nodeAddress.endPointAddressToByteString
    when (nid /= workerNodeId) $ do
        throwString $ "worker unexpected node id: " ++ show nid
    runProcess node $ do
        pid <- getSelfPid
        say $ "worker process id: " ++ show pid
        forever $ do
            liftIO $ threadDelay 10000000
            -- msg :: String <- expect
            -- liftIO $ putStrLn $ "worker msg: " ++ show msg
