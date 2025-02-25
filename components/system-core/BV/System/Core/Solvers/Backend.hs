module BV.System.Core.Solvers.Backend
    ( OfflineSolverBackend
    , OfflineSolverCommandName
    , OfflineSolverConfig (..)
    , OfflineSolverSingleBackend
    , OnlineSolverBackend
    , OnlineSolverConfig (..)
    , SolverBackend (..)
    , SolverCommand (..)
    , SolverGate
    , localSolverBackend
    ) where

import BV.Core
import BV.Logging
import BV.SMTLIB2
import BV.SMTLIB2.Command
import BV.System.Core.Types
import BV.System.Core.Utils.Logging
import BV.System.Utils.Stopwatch

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Binary (Binary)
import Data.List (genericIndex)
import GHC.Generics (Generic)
import System.Process (CreateProcess, proc)
import Text.Printf (printf)

type SolverGate m = forall a. Integer -> m a -> m a

data SolverBackend m
  = SolverBackend
      { online :: OnlineSolverBackend m
      , offline :: OfflineSolverBackend m
      , offlineSingle :: OfflineSolverSingleBackend m
      }
  deriving (Generic)

type OnlineSolverBackend m
    = OnlineSolverConfig
    -> CheckSubgroup
    -> m (Either OnlineSolverFailureInfo ())

type OfflineSolverBackend m
    = OfflineSolverConfig
    -> CheckSubgroup
    -> m (Maybe SatResult)

type OfflineSolverSingleBackend m
    = OfflineSolverConfig
    -> Check
    -> m (Maybe SatResult)

data OnlineSolverConfig
  = OnlineSolverConfig
      { command :: SolverCommand
      , modelConfig :: ModelConfig
      , timeout :: SolverTimeout
      }
  deriving (Eq, Generic, Ord, Show)

instance Binary OnlineSolverConfig where

data OfflineSolverConfig
  = OfflineSolverConfig
      { commandName :: OfflineSolverCommandName
      , command :: SolverCommand
      , modelConfig :: ModelConfig
      , timeout :: SolverTimeout
      }
  deriving (Eq, Generic, Ord, Show)

instance Binary OfflineSolverConfig where

data SolverCommand
  = SolverCommand
      { path :: String
      , args :: [String]
      }
  deriving (Eq, Generic, Ord, Show)

instance Binary SolverCommand where

type OfflineSolverCommandName = String

localSolverBackend :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m) => SolverBackend m
localSolverBackend = SolverBackend
    { online = localOnlineSolverBackend
    , offline = localOfflineSolverBackend
    , offlineSingle = localOfflineSolverSingleBackend
    }

localOnlineSolverBackend
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverBackend m
localOnlineSolverBackend config subgroup = do
    withPushLogContext "online" . withPushLogContextCheckGroup subgroup.group $ do
        logDebug "running solver"
        result <- runSolverWithLogging
            (solverProc config.command)
            (executeSMTProofCheckGroupOnline
                (Just config.timeout)
                config.modelConfig
                (toCoreCheckGroup subgroup))
        logOnlineSolverResult subgroup result
        return result

logOnlineSolverResult :: MonadLoggerWithContext m => CheckSubgroup -> Either OnlineSolverFailureInfo () -> m ()
logOnlineSolverResult subgroup result = do
    case result of
        Right () -> do
            logDebug "answered unsat for all checks"
        Left abort -> do
            logDebug $ printf "answered sat for %d checks" abort.index
            let (_i, check) = subgroup.checks `genericIndex` abort.index
            withPushLogContextCheck check $ do
                case abort.reason of
                    OnlineSolverTimedOut -> do
                        logDebug "timeout"
                    OnlineSolverAnsweredSat -> do
                        logDebug "answered sat"
                    OnlineSolverAnsweredUnknown reason -> do
                        logDebug $ "answered unknown: " ++ showSExpr reason

localOfflineSolverBackend
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OfflineSolverBackend m
localOfflineSolverBackend config subgroup = do
    withPushLogContext "offline" .
        withPushLogContextCheckGroup subgroup.group .
            withPushLogContextOfflineSolver config $ do
                logDebug "running solver"
                (result, elapsed) <- time $ runSolverWithLogging
                    (solverProc config.command)
                    (executeSMTProofCheckGroupOffline
                        (Just config.timeout)
                        config.modelConfig
                        (toCoreCheckGroup subgroup))
                logOfflineSolverResult result elapsed
                return result

localOfflineSolverSingleBackend
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OfflineSolverSingleBackend m
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
                        (toCoreCheck check))
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
