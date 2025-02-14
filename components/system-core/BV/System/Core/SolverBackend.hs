module BV.System.Core.SolverBackend
    ( OfflineSolverCommandName
    , OfflineSolverConfig (..)
    , OfflineSolverGroupConfig (..)
    , OfflineSolversAbortIndex (..)
    , OfflineSolversAbortInfo (..)
    , OfflineSolversAbortReason (..)
    , OfflineSolversConfig (..)
    , OnlineSolverConfig (..)
    , SolverScope (..)
    , prettySolverScope
    , runOfflineSoversBackend
    , runOfflineSoversBackendForSingleCheck
    , runOnlineSolverBackend
    ) where

import BV.Core.ExecuteSMTProofChecks
import BV.Core.Types
import BV.Logging
import BV.SMTLIB2
import BV.SMTLIB2.Command
import BV.System.Core.SolverBackend.ParallelSolvers
import BV.System.Core.Utils.Logging
import BV.System.Core.WithFingerprints
import BV.System.Utils.Stopwatch

import Control.Applicative (empty)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Foldable (for_)
import Data.List (genericIndex)
import GHC.Generics (Generic)
import System.Process (CreateProcess)
import Text.Printf (printf)

--

data OnlineSolverConfig
  = OnlineSolverConfig
      { command :: CreateProcess
      , modelConfig :: ModelConfig
      , timeout :: SolverTimeout
      }
  deriving (Eq, Generic, Show)

type OnlineSolverBackend m a = SMTProofCheckSubgroupWithFingerprints a -> m (Either OnlineSolverAbortInfo ())

runOnlineSolverBackend
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverConfig -> OnlineSolverBackend m a
runOnlineSolverBackend config subgroup = do
    withPushLogContext "online" . withPushLogContextCheckSubgroup subgroup $ do
        logDebug "running solver"
        result <- runSolverWithLogging
            config.command
            (executeSMTProofCheckGroupOnline
                (Just config.timeout)
                config.modelConfig
                subgroup.inner)
        logOnlineSolverResult subgroup result
        return result

logOnlineSolverResult :: MonadLoggerWithContext m => SMTProofCheckSubgroupWithFingerprints a -> Either OnlineSolverAbortInfo () -> m ()
logOnlineSolverResult subgroup result = do
    case result of
        Right () -> do
            logDebug "answered sat for all checks"
        Left abort -> do
            logDebug $ printf "answered sat for %d checks" abort.index
            let fingerprint = (subgroup.inner.imps `genericIndex` abort.index).meta.fingerprint
            withPushLogContextCheckFingerprint fingerprint $ do
                case abort.reason of
                    OnlineSolverAbortReasonTimeout -> do
                        logDebug "timeout"
                    OnlineSolverAbortReasonAnsweredSat -> do
                        logDebug "answered sat"
                    OnlineSolverAbortReasonAnsweredUnknown reason -> do
                        logDebug $ "answered unknown: " ++ showSExpr reason

--

type OfflineSolversBackend m a = SMTProofCheckSubgroupWithFingerprints a -> m (Either OfflineSolversAbortInfo ())

type OfflineSolversBackendForSingleCheck m a = SMTProofCheckWithFingerprint a -> m (Either OfflineSolversAbortReason ())

data OfflineSolversConfig
  = OfflineSolversConfig
      { groups :: [OfflineSolverGroupConfig]
      , timeout :: SolverTimeout
      }
  deriving (Eq, Generic, Show)

data OfflineSolverGroupConfig
  = OfflineSolverGroupConfig
      { commandName :: OfflineSolverCommandName
      , command :: CreateProcess
      , scopes :: [SolverScope]
      , modelConfigs :: [ModelConfig]
      }
  deriving (Eq, Generic, Show)

type OfflineSolverCommandName = String

data OfflineSolverConfig
  = OfflineSolverConfig
      { commandName :: String
      , command :: CreateProcess
      , modelConfig :: ModelConfig
      }
  deriving (Eq, Generic, Show)

data SolverScope
  = SolverScopeHyp
  | SolverScopeAll
  deriving (Eq, Generic, Ord, Show)

prettySolverScope :: SolverScope -> String
prettySolverScope = \case
    SolverScopeAll -> "all"
    SolverScopeHyp -> "hyp"

offlineSolverConfigsForScope :: SolverScope -> [OfflineSolverGroupConfig] -> [OfflineSolverConfig]
offlineSolverConfigsForScope scope groups = flip concatMap groups $
    \OfflineSolverGroupConfig { commandName, command, scopes, modelConfigs } -> do
        unless (scope `elem` scopes) empty
        OfflineSolverConfig commandName command <$> modelConfigs

offlineSolverConfigsForSingleCheck :: [OfflineSolverGroupConfig] -> [OfflineSolverConfig]
offlineSolverConfigsForSingleCheck groups = flip concatMap groups $
    \OfflineSolverGroupConfig { commandName, command, scopes, modelConfigs } -> do
        when (null scopes) empty
        OfflineSolverConfig commandName command <$> modelConfigs

data OfflineSolversAbortInfo
  = OfflineSolversAbortInfo
      { index :: OfflineSolversAbortIndex
      , reason :: OfflineSolversAbortReason
      }
  deriving (Eq, Generic, Ord, Show)

data OfflineSolversAbortIndex
  = OfflineSolversAbortIndexAll
  | OfflineSolversAbortIndexHyp Integer
  deriving (Eq, Generic, Ord, Show)

data OfflineSolversAbortReason
  = OfflineSolversAbortReasonSomeSolverAnsweredSat OfflineSolverCommandName ModelConfig
  | OfflineSolversAbortReasonAllSolversTimedOutOrAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

-- TODO use stm to record successfully checked hyps
runOfflineSoversBackend
    :: forall m a. (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OfflineSolversConfig -> OfflineSolversBackend m a
runOfflineSoversBackend config subgroup = runParallelSolvers $
    mapParallelSolvers (withPushLogContext "offline" . withPushLogContextCheckSubgroup subgroup) $
        mapParallelSolversResult h (all <> hyp)
  where
    h :: SolverResult (Either OfflineSolversAbortInfo ()) () -> SolverResult OfflineSolversAbortInfo ()
    h = undefined
    all :: ParallelSolvers (Either OfflineSolversAbortInfo ()) m ()
    all =
        flip foldMap (offlineSolverConfigsForScope SolverScopeAll config.groups) $ \solver -> liftSolver $ do
            withPushLogContextOfflineSolver solver $ do
                logDebug "running solver"
                (result, elapsed) <- time $ runSolverWithLogging
                    solver.command
                    (executeSMTProofCheckGroupOffline
                        (Just config.timeout)
                        solver.modelConfig
                        subgroup.inner)
                logOfflineSolverResult result elapsed
                return $ case result of
                    Just Sat -> Conclusive $
                        Left (f $ OfflineSolversAbortReasonSomeSolverAnsweredSat solver.commandName solver.modelConfig)
                    Just Unsat -> Conclusive $
                        Right ()
                    _ -> Inconclusive ()
    f = OfflineSolversAbortInfo OfflineSolversAbortIndexAll
    hyp :: ParallelSolvers (Either OfflineSolversAbortInfo ()) m ()
    hyp =
        for_ (zip [0..] (ungroupSMTProofCheckGroup subgroup.inner)) $ \(i, check) ->
            flip foldMap (offlineSolverConfigsForScope SolverScopeHyp config.groups) $ \solver -> liftSolver $ do
                withPushLogContextOfflineSolver solver $ do
                    logDebug "running solver"
                    (result, elapsed) <- time $ runSolverWithLogging
                        solver.command
                        (executeSMTProofCheckOffline
                            (Just config.timeout)
                            solver.modelConfig
                            check)
                    logOfflineSolverResult result elapsed
                    let g = OfflineSolversAbortInfo (OfflineSolversAbortIndexHyp i)
                    return $ case result of
                        Just Sat -> Conclusive $
                            Left (f $ OfflineSolversAbortReasonSomeSolverAnsweredSat solver.commandName solver.modelConfig)
                        Just Unsat -> Conclusive $
                            Right ()
                        _ -> Inconclusive ()

runOfflineSoversBackendForSingleCheck
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OfflineSolversConfig -> OfflineSolversBackendForSingleCheck m a
runOfflineSoversBackendForSingleCheck config check = runParallelSolvers $
    mapParallelSolvers (withPushLogContext "offline" . withPushLogContextCheck check) $
        mapParallelSolversResult f $ flip foldMap solvers $ \solver -> liftSolver $ do
            withPushLogContextOfflineSolver solver $ do
                logDebug "running solver"
                (result, elapsed) <- time $ runSolverWithLogging
                    solver.command
                    (executeSMTProofCheckOffline
                        (Just config.timeout)
                        solver.modelConfig
                        check)
                logOfflineSolverResult result elapsed
                return $ case result of
                    Just Sat -> Conclusive $
                        Left (OfflineSolversAbortReasonSomeSolverAnsweredSat solver.commandName solver.modelConfig)
                    Just Unsat -> Conclusive $
                        Right ()
                    _ -> Inconclusive ()
  where
    solvers = offlineSolverConfigsForSingleCheck config.groups
    f (Inconclusive ()) = Conclusive OfflineSolversAbortReasonAllSolversTimedOutOrAnsweredUnknown
    f (Conclusive (Right ())) = Inconclusive ()
    f (Conclusive (Left abort)) = Conclusive abort

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

withPushLogContextOfflineSolver :: MonadLoggerWithContext m => OfflineSolverConfig -> m a -> m a
withPushLogContextOfflineSolver solver = withPushLogContext ("solver " ++ solver.commandName ++ " " ++ memMode)
  where
    memMode = case solver.modelConfig.memoryMode of
        SolverMemoryModeWord8 -> "word8"
        SolverMemoryModeWord32 -> "word32"

makeElapsedSuffix :: Elapsed -> String
makeElapsedSuffix elapsed = printf " (%.2fs)" (fromRational (elapsedToSeconds elapsed) :: Double)
