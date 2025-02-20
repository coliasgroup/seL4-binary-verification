{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Work
    ( remote
    ) where

import Control.Distributed.Process
import qualified Control.Distributed.Process.Backend.SimpleLocalnet as S
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Exception.Safe
import Control.Monad (forever, when)
import Network.Socket
import Network.Transport
import qualified Network.Transport as NT (Transport)
import qualified Network.Transport.TCP as NT (TCPAddr (Addressable),
                                              TCPAddrInfo (TCPAddrInfo),
                                              createTransport,
                                              defaultTCPParameters)
import System.Posix.Process (getProcessID)

-- remote :: Int -> Process ()
-- remote i = do
remote :: String -> Process ()
remote s = do
    say $ "remote xxx: " ++ s
    -- error $ "GOOOD " ++ s
    upid <- liftIO getProcessID
    -- error "fdshkjlfsd"
    -- liftIO $ putStrLn $ "remote i: " ++ show i
    say $ "remote xxx: " ++ s
    say $ "remote upid: " ++ show upid
