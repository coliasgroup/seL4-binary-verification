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

import Control.Distributed.Process (NodeId, Process, RemoteTable, spawn)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently)
import Control.Exception.Safe (MonadMask)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Binary (Binary)
import qualified Data.Map as M
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
  -- { stagesInput :: StagesInput
  -- }
  deriving (Eq, Generic, Ord, Show)

instance Binary ServerInput where

server :: ServerInput -> Process ()
server input = do
    liftIO $ hPutStr stderr "fjaklfdsla\n"
    liftIO $ forever $ threadDelay maxBound
    -- undefined

remotable ['server]

distribRemoteTable :: RemoteTable
distribRemoteTable = __remoteTable initRemoteTable

withBackend :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m) => DistribConfig -> (SolverBackend m -> m a) -> m a
withBackend config f = do
    node <- liftIO $ newLocalNode config.transport distribRemoteTable
    liftIO $ forConcurrently (M.toList config.workers) $ \(nid, workerConfig) -> do
        runProcess node $ do
            let serverInput = ServerInput
                    -- { stagesInput = config.stagesInput
                    -- }
            pid <- spawn nid ($(mkClosure 'server) serverInput)
            return ()
    liftIO $ forever $ threadDelay maxBound

runDistrib
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => DistribConfig -> SolversConfig -> Checks -> m Report
runDistrib config solversConfig checks = do
    gate <- liftIO $ newSemGate $ sumOf (folded % #numJobs) config.workers
    withBackend config $ \backend -> do
        frontend (applySemGate gate) backend solversConfig checks
