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

newtype TSemN
  = TSemN (TVar Integer)
  deriving (Eq, Typeable)

newTSemN :: Integer -> STM TSemN
newTSemN n = TSemN <$> (newTVar $! n)

waitTSemN :: TSemN -> Integer -> STM ()
waitTSemN (TSemN t) n = do
    m <- readTVar t
    when (n > m) retry
    writeTVar t $! (m - n)

signalTSemN :: TSemN -> Integer -> STM ()
signalTSemN (TSemN t) n = do
    case compare n 0 of
        LT -> throwString "n < 0"
        EQ -> return ()
        GT -> modifyTVar' t (+ n)
