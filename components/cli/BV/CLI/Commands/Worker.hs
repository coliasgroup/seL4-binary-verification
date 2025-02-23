module BV.CLI.Commands.Worker
    ( runWorker
    ) where

import BV.CLI.Distrib
import BV.CLI.Opts
import BV.System.Distrib

import Network.Transport
import Network.Transport.Static
import Network.Transport.Static.Utils

import Control.Concurrent (threadDelay)
import Control.Distributed.Process.Node (newLocalNode)
import Control.Exception.Safe (MonadThrow)
import Control.Monad (forever)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import qualified Data.ByteString.Char8 as C

runWorker :: (MonadThrow m, MonadUnliftIO m) => WorkerOpts -> m ()
runWorker opts = liftIO $ do
    withStaticTransport workerAddr (workerPeers driverAddr) $ \transport -> do
        _node <- newLocalNode transport distribRemoteTable
        forever (threadDelay maxBound)
  where
    workerAddr = EndPointAddress (C.pack opts.addr)
