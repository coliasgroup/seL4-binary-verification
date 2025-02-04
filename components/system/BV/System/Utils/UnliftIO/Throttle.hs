module BV.System.Utils.UnliftIO.Throttle
    ( withThrottleUnliftIO
    , withThrottlingUnliftIO
    ) where

import BV.System.Throttle

import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

withThrottlingUnliftIO :: MonadUnliftIO m => Units -> (Throttle -> m a) -> m a
withThrottlingUnliftIO availableUnits f =
    withRunInIO $ \run -> withThrottling availableUnits (run . f)

withThrottleUnliftIO :: MonadUnliftIO m => Throttle -> Priority -> Units -> m a -> m a
withThrottleUnliftIO throttle priority units m =
    withRunInIO $ \run -> withThrottle throttle priority units (run m)
