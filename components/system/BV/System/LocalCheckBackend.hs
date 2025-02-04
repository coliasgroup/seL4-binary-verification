{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}

module BV.System.LocalCheckBackend
    ( AcceptableSatResult (..)
    , LocalCheckBackendConfig (..)
    , localCheckBackend
    ) where

import BV.Core.AdornProofScript
import BV.Core.ExecuteSMTProofChecks
import BV.Core.Stages
import BV.Core.Types
import BV.SMTLIB2.Builder
import BV.SMTLIB2.Process
import BV.SMTLIB2.Types
import BV.System.BackendCore
import BV.System.Cache
import BV.System.Fingerprinting
import BV.System.Frontend
import BV.System.SolversConfig
import BV.System.TaskQueue
import BV.System.Throttle
import BV.System.Utils.Logger
import BV.System.Utils.UnliftIO.Async
import BV.System.Utils.UnliftIO.Throttle

import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently),
                                 race)
import Control.Monad (filterM, forM_, forever, unless)
import Control.Monad.Catch (MonadMask, MonadThrow, SomeException)
import Control.Monad.Except (ExceptT (ExceptT), MonadError, runExceptT,
                             throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.State (evalState, state)
import Control.Monad.Trans (lift)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes, fromJust)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Lazy.Builder
import Data.Void (absurd)
import GHC.Generics (Generic)
import Optics
import System.Process (CreateProcess, proc)
import Text.Printf (printf)

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
