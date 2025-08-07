module BV.CLI.Commands.Worker
    ( runWorker
    ) where

import BV.CLI.Distrib
import BV.CLI.Opts
import BV.System.Utils.Distrib (runProcess')

import Network.Transport.Static (withStaticTransport)
import Network.Transport.Static.Utils (workerPeers)

import Control.Distributed.Process.Node (newLocalNode)
import qualified Data.ByteString.Char8 as C
import Network.Transport (EndPointAddress (EndPointAddress))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr)

-- TODO
-- "thread blocked indefinitely in an MVar operation", from somewhere in distributed-process
runWorker :: WorkerOpts -> IO ()
runWorker opts = do
    hSetBuffering stderr LineBuffering
    withStaticTransport workerAddr (workerPeers driverAddr) $ \transport -> do
        node <- newLocalNode transport distribRemoteTable
        cond <- runProcess' node $ watchdog driverNodeId
        print cond
  where
    workerAddr = EndPointAddress (C.pack opts.addr)
