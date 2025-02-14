module BV.System.Core.SolverFrontend
    ( CheckFilter (..)
    , SolverBackend (..)
    , frontend
    ) where

import BV.Core.DecorateProofScript
import BV.Core.ExecuteSMTProofChecks
import BV.Core.Types
import BV.Logging
import BV.SMTLIB2
import BV.SMTLIB2.Command
import BV.System.Cache
import BV.System.Core.Fingerprinting
import BV.System.Core.Report
import BV.System.Core.SolverBackend
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

data SolverBackend m a
  = SolverBackend
      { online :: OnlineSolverBackend m a
      , offline :: OfflineSolverBackend m a
      , offlineForSingleCheck :: OfflineSolverBackendForSingleCheck m a
      }
  deriving (Generic)

solversFrontend
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => SolverBackend m i -> SMTProofCheckSubgroupWithFingerprints i -> m (SMTProofCheckResult i ())
solversFrontend backend subgroup = undefined

data CheckFilter
  = CheckFilter
      { pairings :: PairingId -> Bool
      , groups :: SMTProofCheckGroupFingerprint -> Bool
      , checks :: SMTProofCheckFingerprint -> Bool
      }
  deriving (Generic)

frontend
    :: ( MonadUnliftIO m
       , MonadLoggerWithContext m
       , MonadCache m
       , MonadMask m
       )
    => SolverBackend m SMTProofCheckDescription
    -> CheckFilter
    -> PreparedSMTProofChecksWithFingerprints
    -> m Report
frontend backend filter checks = do
    undefined
