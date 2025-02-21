{-# LANGUAGE OverloadedStrings #-}

module TCP where

import Network.Transport

import Control.Exception.Safe
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLoggerIO)
import Network.Socket
import qualified Network.Transport.TCP as NT (TCPAddr (Addressable),
                                              TCPAddrInfo (TCPAddrInfo),
                                              createTransport,
                                              defaultTCPParameters)

driverAddr :: EndPointAddress
driverAddr = EndPointAddress "localhost:8080:0"

workerAddr :: EndPointAddress
workerAddr = EndPointAddress "localhost:8081:0"

mkTransport :: HostName -> ServiceName -> IO Transport
mkTransport host port = do
    r <- NT.createTransport (NT.Addressable (NT.TCPAddrInfo host port (host,))) NT.defaultTCPParameters
    case r of
        Left err -> throwString $ show err
        Right t -> return t

withDriverTransport :: (MonadUnliftIO m, MonadLoggerIO m, MonadThrow m) => (Transport -> m a) -> m a
withDriverTransport f = do
    let host = "localhost"
    let port = "8080"
    transport <- liftIO $ mkTransport host port
    f transport

withWorkerTransport :: (MonadUnliftIO m, MonadLoggerIO m, MonadThrow m) => (Transport -> m a) -> m a
withWorkerTransport f = do
    let host = "localhost"
    let port = "8081"
    transport <- liftIO $ mkTransport host port
    f transport
