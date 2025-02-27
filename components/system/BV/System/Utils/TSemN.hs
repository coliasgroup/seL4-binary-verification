module BV.System.Utils.TSemN
    ( TSemN
    , newTSemN
    , signalTSemN
    , waitTSemN
    ) where

import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, retry, writeTVar)
import Control.Exception.Safe (throwString)
import Control.Monad (when)
import Data.Typeable (Typeable)

newtype TSemN
  = TSemN (TVar Integer)
  deriving (Eq, Typeable)

newTSemN :: Integer -> STM TSemN
newTSemN i = TSemN <$> (newTVar $! i)

waitTSemN :: TSemN -> Integer -> STM ()
waitTSemN (TSemN t) n = do
    i <- readTVar t
    when (i < n) retry
    writeTVar t $! (i - n)

signalTSemN :: TSemN -> Integer -> STM ()
signalTSemN (TSemN t) n = do
    case compare n 0 of
        LT -> throwString "n < 0"
        EQ -> return ()
        GT -> do
            i <- readTVar t
            writeTVar t $! (i + n)
