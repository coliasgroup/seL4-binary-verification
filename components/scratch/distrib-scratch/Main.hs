module Main
    ( main
    ) where

import Opts

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forever, forM_)

main :: IO ()
main = do
    opts <- parseOpts
    case opts of
        CommandDriver opts' -> runDriver opts'
        CommandWorker opts' -> runWorker opts'

runDriver :: DriverOpts -> IO ()
runDriver opts = do
    let host = "localhost"
    let port = "9001"
    putStrLn "run driver"
    backend <- initializeBackend host port initRemoteTable
    startMaster backend (master backend)

runWorker :: WorkerOpts -> IO ()
runWorker opts = do
    let host = "localhost"
    let port = "9002"
    putStrLn "run worker"
    backend <- initializeBackend host port initRemoteTable
    startSlave backend

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
    -- Do something interesting with the slaves
    liftIO . putStrLn $ "Slaves: " ++ show slaves
    -- Terminate the slaves when the master terminates (this is optional)
    terminateAllSlaves backend
