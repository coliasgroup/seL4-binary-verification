module BV.System.Utils.UnliftIO
    ( withRunInMonadIO
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

withRunInMonadIO :: MonadUnliftIO m => (forall n. MonadIO n => (forall a. m a -> n a) -> n b) -> m b
withRunInMonadIO f = withRunInIO $ \run -> f (liftIO . run)
