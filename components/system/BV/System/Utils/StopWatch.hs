{-# OPTIONS_GHC -Wno-type-defaults #-}

module BV.System.Utils.StopWatch
    ( Elapsed
    , StopWatch
    , elapsedToSeconds
    , getElapsed
    , newStopWatch
    , time
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Ratio ((%))
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Word (Word64)

newtype StopWatch
  = StopWatch { startNSec :: Word64 }
  deriving (Eq, Ord, Show)

newtype Elapsed
  = Elapsed { nsecs :: Word64 }
  deriving (Eq, Ord, Show)

newStopWatch :: MonadIO m => m StopWatch
newStopWatch = StopWatch <$> liftIO getMonotonicTimeNSec

getElapsed :: MonadIO m => StopWatch -> m Elapsed
getElapsed sw = do
    now <- liftIO getMonotonicTimeNSec
    return . Elapsed $ now - sw.startNSec

elapsedToSeconds :: Elapsed -> Rational
elapsedToSeconds elapsed = toInteger elapsed.nsecs % (10^9)

time :: MonadIO m => m a -> m (a, Elapsed)
time m = do
    stopWatch <- newStopWatch
    a <- m
    elapsed <- getElapsed stopWatch
    return (a, elapsed)
