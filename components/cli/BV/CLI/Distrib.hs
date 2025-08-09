{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.CLI.Distrib where

import qualified BV.System.Distrib (__remoteTable)

import Control.Distributed.Process

import Control.Concurrent (setNumCapabilities, threadDelay)
import qualified Control.Distributed.Process.Async as A
import Control.Distributed.Process.Closure (functionTDict, mkClosure, remotable)
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Monad (forever)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans (lift)
import Data.Binary (Binary)
import Data.Void (absurd)
import GHC.Conc (getNumProcessors)
import GHC.Generics (Generic)
import Network.Transport (EndPointAddress (EndPointAddress))

driverAddr :: EndPointAddress
driverAddr = EndPointAddress "driver"

driverNodeId :: NodeId
driverNodeId = NodeId driverAddr

threadDelayClosureFn :: Int -> Process ()
threadDelayClosureFn = liftIO . threadDelay

getNumProcessorsClosureFn :: () -> Process Int
getNumProcessorsClosureFn () = liftIO getNumProcessors

setNumCapabilitiesClosureFn :: Int -> Process ()
setNumCapabilitiesClosureFn = liftIO . setNumCapabilities

remotable ['threadDelayClosureFn, 'getNumProcessorsClosureFn, 'setNumCapabilitiesClosureFn]

distribRemoteTable :: RemoteTable
distribRemoteTable =
      __remoteTable
    . BV.System.Distrib.__remoteTable
    $ initRemoteTable

data WatchdogCondition
  = WatchdogConditionNodeMonitorNotification NodeMonitorNotification
  | WatchdogConditionTimeout
  | WatchdogConditionUnexpectedResult (A.AsyncResult ())
  deriving (Generic, Show)

instance Binary WatchdogCondition

watchdog :: NodeId -> Process WatchdogCondition
watchdog nid = do
    mon <- A.async $ A.task $ WatchdogConditionNodeMonitorNotification <$> (monitorNode nid >> expect)
    timer <- A.async $ A.task $ watchdogTimer nid
    (_, r) <- A.waitAnyCancel [mon, timer]
    let A.AsyncDone cond = r
    return cond

watchdogTimer :: NodeId -> Process WatchdogCondition
watchdogTimer nid = either id absurd <$> runExceptT (forever check)
  where
    delay = 60 * 1_000_000
    timeout = delay * 2
    check = do
        a <- lift $ A.asyncLinked $ A.remoteTask
            $(functionTDict 'threadDelayClosureFn)
            nid
            ($(mkClosure 'threadDelayClosureFn) delay)
        lift (A.waitTimeout timeout a) >>= \case
            Just (A.AsyncDone ()) -> return ()
            Nothing -> throwError WatchdogConditionTimeout
            Just r -> throwError $ WatchdogConditionUnexpectedResult r
