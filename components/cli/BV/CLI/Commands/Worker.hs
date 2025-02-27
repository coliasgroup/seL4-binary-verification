module BV.CLI.Commands.Worker
    ( runWorker
    ) where

import BV.CLI.Distrib
import BV.CLI.Opts
import BV.System.Distrib

import Network.Transport
import Network.Transport.Static
import Network.Transport.Static.Utils

import Control.Distributed.Process (NodeMonitorNotification (..), expect,
                                    monitorNode)
import Control.Distributed.Process.Node (newLocalNode, runProcess)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import qualified Data.ByteString.Char8 as C

-- TODO
-- "thread blocked indefinitely in an MVar operation", from somewhere in distributed-process
runWorker :: (MonadThrow m, MonadUnliftIO m) => WorkerOpts -> m ()
runWorker opts = liftIO $ do
    withStaticTransport workerAddr (workerPeers driverAddr) $ \transport -> do
        node <- newLocalNode transport distribRemoteTable
        runProcess node $ do
            -- linkNode driverNodeId
            monitorNode driverNodeId
            NodeMonitorNotification _ _ _reason <- expect
            -- liftIO $ hPrint stderr reason
            return ()
  where
    workerAddr = EndPointAddress (C.pack opts.addr)
