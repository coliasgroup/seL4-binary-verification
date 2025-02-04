{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}

module BV.System.Backend.Local
    ( LocalBackendConfig (..)
    , localBackend
    ) where

import BV.Core.Stages
import BV.System.Backend.Core
import BV.System.Cache
import BV.System.Frontend
import BV.System.Throttle
import BV.System.Utils.Logger
import BV.System.Utils.UnliftIO.Throttle

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import GHC.Generics (Generic)

data LocalBackendConfig
  = LocalBackendConfig
      { numCores :: Integer
      , backendCoreConfig :: BackendCoreConfig
      }
  deriving (Eq, Generic, Ord, Show)

localBackend
    :: (MonadUnliftIO m, MonadLoggerAddContext m, MonadCache m, MonadMask m)
    => LocalBackendConfig -> PreparedSMTProofChecks -> m Report
localBackend config checks = do
    withThrottlingUnliftIO (Units config.numCores) $ \throttle -> do
        frontend (backendCore config.backendCoreConfig throttle) checks
