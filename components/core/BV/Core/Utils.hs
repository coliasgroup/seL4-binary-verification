module BV.Core.Utils
    ( whenJustThen
    , whenNothing
    ) where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT), hoistMaybe)

whenNothing :: Monad m => Maybe a -> m a -> m a
whenNothing opt m = maybe m return opt

whenJustThen :: Monad m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
whenJustThen opt f = runMaybeT $ hoistMaybe opt >>= MaybeT . f
