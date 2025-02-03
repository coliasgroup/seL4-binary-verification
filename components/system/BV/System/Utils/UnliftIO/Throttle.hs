module BV.System.Utils.UnliftIO.Throttle
    ( withThrottlingUnliftIO
    ) where

import BV.System.Throttle

import Control.Monad.IO.Unlift

withThrottlingUnliftIO :: MonadUnliftIO m => Units -> (Throttle -> m a) -> m a
withThrottlingUnliftIO availableUnits f = withRunInIO $ \run -> withThrottling availableUnits (run . f)
