{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}

module BV.System.Backend.Local
    ( AcceptableSatResult (..)
    , LocalCheckBackendConfig (..)
    , localCheckBackend
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

data LocalCheckBackendConfig
  = LocalCheckBackendConfig
      { numCores :: Integer
      , backendCoreConfig :: BackendCoreConfig
      }
  deriving (Eq, Generic, Ord, Show)

localCheckBackend
    :: (MonadUnliftIO m, MonadLoggerAddContext m, MonadCache m, MonadMask m)
    => LocalCheckBackendConfig -> PreparedSMTProofChecks -> m Report
localCheckBackend config checks = do
    withThrottlingUnliftIO (Units config.numCores) $ \throttle -> do
        frontend (backendCore config.backendCoreConfig throttle) checks
