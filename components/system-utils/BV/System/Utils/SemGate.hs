module BV.System.Utils.SemGate
    ( SemGate
    , applySemGate
    , newSemGate
    ) where

import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVar,
                               readTVar, retry, writeTVar)
import Control.Exception.Safe (bracket_, throwString)
import Control.Monad (when)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIOOp)
import Data.Typeable (Typeable)

newtype SemGate
  = SemGate TSemN

newSemGate :: Integer -> IO SemGate
newSemGate width = SemGate <$> atomically (newTSemN width)

applySemGate :: MonadUnliftIO m => SemGate -> Integer -> m a -> m a
applySemGate (SemGate sem) n = liftIOOp $ bracket_ (atomically (waitTSemN sem n)) (atomically (signalTSemN sem n))

--

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
