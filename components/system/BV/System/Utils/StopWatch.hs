{-# OPTIONS_GHC -Wno-type-defaults #-}

module BV.System.Utils.StopWatch
    ( Elapsed
    , StopWatch
    , elapsedToSeconds
    , getElapsed
    , newStopWatch
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Word (Word64)
import Data.Ratio ((%))

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
