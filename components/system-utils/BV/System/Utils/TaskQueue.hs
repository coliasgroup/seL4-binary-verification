module BV.System.Utils.TaskQueue
    ( SupportsTask (..)
    , Task
    , TaskQueueControl
    , TaskQueueIn
    , acceptTask
    , newTaskQueue
    , submitTaskAndWait
    , useTask
    ) where

import Control.Concurrent (Chan, MVar, newChan, newEmptyMVar, readChan,
                           readMVar, tryPutMVar, writeChan)
import Control.Monad (unless)
import GHC.Generics (Generic)

data Task i o
  = Task
      { input :: i
      , output :: MVar o
      }
  deriving (Eq, Generic)

class SupportsTask i o t | t i -> o where
    wrapTask :: Task i o -> t

instance SupportsTask i o (Task i o) where
    wrapTask = id

data TaskQueueIn t
  = TaskQueueIn
      { chan :: Chan t
      }

data TaskQueueControl t
  = TaskQueueControl
      { chan :: Chan t
      }

newTaskQueue :: IO (TaskQueueIn t, TaskQueueControl t)
newTaskQueue = do
    chan <- newChan
    return (TaskQueueIn chan, TaskQueueControl chan)

submitTaskAndWait :: SupportsTask i o t => TaskQueueIn t -> i -> IO o
submitTaskAndWait taskQueueIn input = do
    output <- newEmptyMVar
    writeChan taskQueueIn.chan (wrapTask (Task { input, output }))
    readMVar output

acceptTask :: TaskQueueControl t -> IO t
acceptTask taskQueueControl = do
    readChan taskQueueControl.chan

useTask :: Task i o -> (i, o -> IO ())
useTask (Task { input, output }) = (input, withOutput)
  where
    withOutput o = do
        success <- tryPutMVar output o
        unless success $ do
            fail "task output returned more than once"
