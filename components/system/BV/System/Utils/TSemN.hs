module BV.System.Utils.TSemN
    ( TSemN
    , newTSemN
    , signalTSemN
    , waitTSemN
    ) where

import Control.Concurrent.STM (STM, TVar, modifyTVar', newTVar, readTVar, retry,
                               writeTVar)
import Control.Exception.Safe (throwString)
import Control.Monad (when)
import Data.Typeable (Typeable)

data TSemN
  = TSemN
      { total :: Integer
      , avail :: TVar Integer
      }
  deriving (Eq, Typeable)

newTSemN :: Integer -> STM TSemN
newTSemN n = TSemN n <$> (newTVar $! n)

waitTSemN :: TSemN -> Integer -> STM ()
waitTSemN sem n = do
    when (n > sem.total) $ throwString "n > total"
    m <- readTVar sem.avail
    when (n > m) retry
    writeTVar sem.avail $! (m - n)

signalTSemN :: TSemN -> Integer -> STM ()
signalTSemN sem n = do
    case compare n 0 of
        LT -> throwString "n < 0"
        EQ -> return ()
        GT -> modifyTVar' sem.avail (+ n)
