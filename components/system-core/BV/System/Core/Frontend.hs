{-# OPTIONS_GHC -Wno-unused-imports #-}

module BV.System.Core.Frontend
    ( frontend
    ) where

import BV.Core
import BV.Logging
import BV.SMTLIB2
import BV.SMTLIB2.Command
import BV.System.Core.Cache
import BV.System.Core.Fingerprinting
import BV.System.Core.Report
import BV.System.Core.Solvers
import BV.System.Core.Utils.Logging
import BV.System.Core.WithFingerprints
import BV.System.Utils.Stopwatch
import BV.System.Utils.UnliftIO.Async

import Control.Applicative (empty)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Foldable (for_)
import Data.List (genericIndex, genericLength)
import GHC.Generics (Generic)
import System.Process (CreateProcess)
import Text.Printf (printf)

frontend
    :: forall m i. (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => (forall a. Integer -> m a -> m a)
    -> SolverBackend m
    -> SolversConfig
    -> PreparedSMTProofChecksWithFingerprints
    -> m Report
frontend throttle backend config checks = do
    undefined
