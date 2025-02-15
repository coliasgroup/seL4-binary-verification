module BV.System.Core.Solvers.Backend
    ( OfflineSolverBackend
    , OfflineSolverCommandName
    , OfflineSolverConfig (..)
    , OfflineSolverSingleBackend
    , OnlineSolverBackend
    , OnlineSolverConfig (..)
    , SolverBackend (..)
    , SolverCommand (..)
    , localSolverBackend
    ) where

import BV.Core
import BV.Logging
import BV.SMTLIB2
import BV.SMTLIB2.Command
import BV.System.Core.Utils.Logging
import BV.System.Core.WithFingerprints
import BV.System.Utils.Stopwatch

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.List (genericIndex)
import GHC.Generics (Generic)
import System.Process (CreateProcess, proc)
import Text.Printf (printf)

data SolverBackend m
  = SolverBackend
      { online :: OnlineSolverBackend () m
      , offline :: OfflineSolverBackend () m
      , offlineSingle :: OfflineSolverSingleBackend (SubgroupElementMeta ()) m
      }
  deriving (Generic)

type OnlineSolverBackend a m
    = OnlineSolverConfig
    -> SMTProofCheckSubgroupWithFingerprints a
    -> m (Either OnlineSolverFailureInfo ())

type OfflineSolverBackend a m
    = OfflineSolverConfig
    -> SMTProofCheckSubgroupWithFingerprints a
    -> m (Maybe SatResult)

type OfflineSolverSingleBackend a m
    = OfflineSolverConfig
    -> SMTProofCheckWithFingerprint a
    -> m (Maybe SatResult)

data OnlineSolverConfig
  = OnlineSolverConfig
      { command :: SolverCommand
      , modelConfig :: ModelConfig
      , timeout :: SolverTimeout
      }
  deriving (Eq, Generic, Ord, Show)

data OfflineSolverConfig
  = OfflineSolverConfig
      { commandName :: OfflineSolverCommandName
      , command :: SolverCommand
      , modelConfig :: ModelConfig
      , timeout :: SolverTimeout
      }
  deriving (Eq, Generic, Ord, Show)

data SolverCommand
  = SolverCommand
      { path :: String
      , args :: [String]
      }
  deriving (Eq, Generic, Ord, Show)

type OfflineSolverCommandName = String

localSolverBackend :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m) => SolverBackend m
localSolverBackend = SolverBackend
    { online = localOnlineSolverBackend
    , offline = localOfflineSolverBackend
    , offlineSingle = localOfflineSolverSingleBackend
    }

localOnlineSolverBackend
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverBackend a m
localOnlineSolverBackend config subgroup = do
    withPushLogContext "online" . withPushLogContextCheckSubgroup subgroup $ do
        logDebug "running solver"
        result <- runSolverWithLogging
            (solverProc config.command)
            (executeSMTProofCheckGroupOnline
                (Just config.timeout)
                config.modelConfig
                subgroup.inner)
        logOnlineSolverResult subgroup result
        return result

logOnlineSolverResult :: MonadLoggerWithContext m => SMTProofCheckSubgroupWithFingerprints a -> Either OnlineSolverFailureInfo () -> m ()
logOnlineSolverResult subgroup result = do
    case result of
        Right () -> do
            logDebug "answered sat for all checks"
        Left abort -> do
            logDebug $ printf "answered sat for %d checks" abort.index
            let fingerprint = (subgroup.inner.imps `genericIndex` abort.index).meta.fingerprint
            withPushLogContextCheckFingerprint fingerprint $ do
                case abort.reason of
                    OnlineSolverTimedOut -> do
                        logDebug "timeout"
                    OnlineSolverAnsweredSat -> do
                        logDebug "answered sat"
                    OnlineSolverAnsweredUnknown reason -> do
                        logDebug $ "answered unknown: " ++ showSExpr reason

localOfflineSolverBackend
    :: forall m a. (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OfflineSolverBackend a m
localOfflineSolverBackend config subgroup = do
    withPushLogContext "offline" .
        withPushLogContextCheckSubgroup subgroup .
            withPushLogContextOfflineSolver config $ do
                logDebug "running solver"
                (result, elapsed) <- time $ runSolverWithLogging
                    (solverProc config.command)
                    (executeSMTProofCheckGroupOffline
                        (Just config.timeout)
                        config.modelConfig
                        subgroup.inner)
                logOfflineSolverResult result elapsed
                return result

localOfflineSolverSingleBackend
    :: forall m a. (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OfflineSolverSingleBackend a m
localOfflineSolverSingleBackend config check = do
    withPushLogContext "offline" .
        withPushLogContextCheck check .
            withPushLogContextOfflineSolver config $ do
                logDebug "running solver"
                (result, elapsed) <- time $ runSolverWithLogging
                    (solverProc config.command)
                    (executeSMTProofCheckOffline
                        (Just config.timeout)
                        config.modelConfig
                        check)
                logOfflineSolverResult result elapsed
                return result

withPushLogContextOfflineSolver :: MonadLoggerWithContext m => OfflineSolverConfig -> m a -> m a
withPushLogContextOfflineSolver solver =
    withPushLogContext ("solver " ++ solver.commandName ++ " " ++ prettyModelConfig solver.modelConfig)

logOfflineSolverResult :: MonadLoggerWithContext m => Maybe SatResult -> Elapsed -> m ()
logOfflineSolverResult result elapsed = do
    case result of
        Nothing -> do
            logDebug "timeout"
        Just Sat -> do
            logDebug $ "answered sat" ++ elapsedSuffix
        Just Unsat -> do
            logDebug $ "answered unsat" ++ elapsedSuffix
        Just (Unknown reason) -> do
            logDebug $ "answered unknown: " ++ showSExpr reason ++ " " ++ elapsedSuffix
  where
    elapsedSuffix = makeElapsedSuffix elapsed

makeElapsedSuffix :: Elapsed -> String
makeElapsedSuffix elapsed = printf " (%.2fs)" (fromRational (elapsedToSeconds elapsed) :: Double)

solverProc :: SolverCommand -> CreateProcess
solverProc cmd = proc cmd.path cmd.args
