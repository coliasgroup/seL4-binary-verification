{-# LANGUAGE OverloadedStrings #-}

module Static where

import BV.Logging
import Network.Transport.Static
import Network.Transport.Static.Utils

import Network.Transport

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_, link, withAsync)
import Control.Exception.Safe
import Control.Monad (forever, unless)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO, withRunInIO)
import Control.Monad.Logger (MonadLoggerIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Foldable (for_)
import qualified Data.Map as M
import System.IO (BufferMode (LineBuffering), hSetBuffering)
import System.Process (CreateProcess (std_err), StdStream (CreatePipe), proc)

driverAddr :: EndPointAddress
driverAddr = EndPointAddress "driver"

workerAddr :: EndPointAddress
workerAddr = EndPointAddress "worker"

withDriverTransport :: (MonadUnliftIO m, MonadLoggerIO m, MonadThrow m) => (Transport -> m a) -> m a
withDriverTransport f = do
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
            withAsync handleStderrs $ \a -> do
                link a
                withStaticTransport driverAddr peers (run . f)

withWorkerTransport :: (MonadUnliftIO m, MonadLoggerIO m, MonadThrow m) => (Transport -> m a) -> m a
withWorkerTransport f = do
    forever $ liftIO $ threadDelay 1000
    let peers = workerPeers driverAddr
    withRunInIO $ \run -> do
        withStaticTransport workerAddr peers (run . f)
