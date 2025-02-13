{-# OPTIONS_GHC -Wno-type-defaults #-}

module BV.System.Utils.Stopwatch
    ( Elapsed
    , Stopwatch
    , elapsedToSeconds
    , getElapsed
    , newStopwatch
    , time
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Ratio ((%))
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Word (Word64)

newtype Stopwatch
  = Stopwatch { startNSec :: Word64 }
  deriving (Eq, Ord, Show)

newtype Elapsed
  = Elapsed { nsecs :: Word64 }
  deriving (Eq, Ord, Show)

newStopwatch :: MonadIO m => m Stopwatch
newStopwatch = Stopwatch <$> liftIO getMonotonicTimeNSec

getElapsed :: MonadIO m => Stopwatch -> m Elapsed
getElapsed sw = do
    now <- liftIO getMonotonicTimeNSec
    return . Elapsed $ now - sw.startNSec

elapsedToSeconds :: Elapsed -> Rational
elapsedToSeconds elapsed = toInteger elapsed.nsecs % (10^9)

time :: MonadIO m => m a -> m (a, Elapsed)
time m = do
    stopwatch <- newStopwatch
    a <- m
    elapsed <- getElapsed stopwatch
    return (a, elapsed)
