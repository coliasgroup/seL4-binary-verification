{-# OPTIONS_GHC -Wno-orphans #-}

module BV.System.Utils.Distrib
    ( runProcess'
    ) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Distributed.Process (kill)
import Control.Distributed.Process.Internal.Types (Process (..))
import Control.Distributed.Process.Node (LocalNode, forkProcess, runProcess)
import Control.Exception.Safe (SomeException, bracket, throwIO, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)

deriving newtype instance MonadUnliftIO Process

runProcess' :: MonadIO m => LocalNode -> Process a -> m a
runProcess' localNode proc = liftIO $ bracket
    (do
        done <- newEmptyMVar
        pid <- forkProcess localNode $ try proc >>= liftIO . putMVar done
        return (done, pid))
    (\(_done, pid) -> do
        runProcess localNode $ kill pid "cancel")
    (\(done, _pid) -> do
        takeMVar done >>= either (throwIO :: SomeException -> IO a) return)
