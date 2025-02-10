{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}

module BV.System.Backend.Local
    ( LocalBackendConfig (..)
    , localBackend
    , localBackendJustTheseChecks
    ) where

import BV.Core.Stages
import BV.Core.Types
import BV.Logging
import BV.System.Backend.Core
import BV.System.Cache
import BV.System.Frontend
import BV.System.SolversConfig
import BV.System.Throttle
import BV.System.Utils.UnliftIO.Throttle
import BV.System.WithFingerprints

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.Map as M
import GHC.Generics (Generic)

data LocalBackendConfig
  = LocalBackendConfig
      { numJobs :: Integer
      , backendCoreConfig :: BackendCoreConfig
      }
  deriving (Eq, Generic, Ord, Show)

localBackend
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => LocalBackendConfig -> PreparedSMTProofChecksWithFingerprints -> m Report
localBackend config checks = do
    withThrottlingUnliftIO (Units config.numJobs) $ \throttle -> do
        frontend (backendCore config.backendCoreConfig throttle) checks

localBackendJustTheseChecks
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => LocalBackendConfig
    -> M.Map PairingId [SMTProofCheckWithFingerprint SMTProofCheckDescription]
    -> m Report
localBackendJustTheseChecks config checks = do
    withThrottlingUnliftIO (Units config.numJobs) $ \throttle -> do
        frontendJustTheseChecks (backendCoreSingleCheck config.backendCoreConfig.solversConfig.offline throttle) checks
