{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BV.System.Cache.Postgres
    ( withPostgresCacheContext
    ) where

import BV.Logging
import BV.System.Core

import Control.Monad.IO.Unlift (MonadUnliftIO)

withPostgresCacheContext :: (MonadUnliftIO m, MonadLoggerWithContext m) => String -> (CacheContext m -> m a) -> m a
withPostgresCacheContext connString f =
    error "TODO"
