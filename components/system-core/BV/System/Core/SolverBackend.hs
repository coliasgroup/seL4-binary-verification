{-# LANGUAGE PatternSynonyms #-}

module BV.System.Core.SolverBackend
    ( OfflineSolverCommandName
    , OfflineSolverConfig (..)
    , OfflineSolverGroupConfig (..)
    , OfflineSolversConfig (..)
    , OfflineSolversFailureIndex (..)
    , OfflineSolversFailureInfo (..)
    , OnlineSolverConfig (..)
    , SolverScope (..)
    , numParallelSolvers
    , numParallelSolversForSingleCheck
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

--

data OnlineSolverConfig
  = OnlineSolverConfig
      { command :: CreateProcess
      , modelConfig :: ModelConfig
      , timeout :: SolverTimeout
      }
  deriving (Eq, Generic, Show)

type OnlineSolverBackend m a = SMTProofCheckSubgroupWithFingerprints a -> m (Either OnlineSolverFailureInfo ())

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

--

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
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

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

numParallelSolvers :: [OfflineSolverGroupConfig] -> Integer
numParallelSolvers groups = sum
    [ genericLength (offlineSolverConfigsForScope scope groups)
    | scope <- [minBound..maxBound]
    ]

numParallelSolversForSingleCheck :: [OfflineSolverGroupConfig] -> Integer
numParallelSolversForSingleCheck groups = genericLength (offlineSolverConfigsForSingleCheck groups)

--

data OfflineSolversFailureInfo i
  = SomeOfflineSolverAnsweredSat OfflineSolverCommandName ModelConfig i
  | AllOfflineSolversTimedOutOrAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

data OfflineSolversFailureIndex
  = OfflineSolversFailureIndexAll
  | OfflineSolversFailureIndexHyp Integer
  deriving (Eq, Generic, Ord, Show)

--

type OfflineSolversBackend m a = SMTProofCheckSubgroupWithFingerprints a -> m (Either (OfflineSolversFailureInfo OfflineSolversFailureIndex) ())

-- TODO use stm to record successfully checked hyps
runOfflineSoversBackend
    :: forall m a. (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OfflineSolversConfig -> OfflineSolversBackend m a
runOfflineSoversBackend config subgroup = do
    withPushLogContext "offline" . withPushLogContextCheckSubgroup subgroup $
        mapConclusion <$> concurrentlyUnliftIOE_ allStrategy hypStrategy
  where
    allStrategy = do
        withPushLogContext "all" $ do
            forConcurrentlyUnliftIOE_ (offlineSolverConfigsForScope SolverScopeAll config.groups) $ \solver -> do
                withPushLogContextOfflineSolver solver $ do
                    logDebug "running solver"
                    (result, elapsed) <- time $ runSolverWithLogging
                        solver.command
                        (executeSMTProofCheckGroupOffline
                            (Just config.timeout)
                            solver.modelConfig
                            subgroup.inner)
                    logOfflineSolverResult result elapsed
                    return $ mapSatResult
                        (SomeOfflineSolverAnsweredSat solver.commandName solver.modelConfig OfflineSolversFailureIndexAll)
                        result
    hypStrategy = do
        withPushLogContext "hyp" $ do
            runExceptT $ do
                for_ (zip [0..] (ungroupSMTProofCheckGroup subgroup.inner)) $ \(i, check) ->
                    ExceptT . forConcurrentlyUnliftIOE_ (offlineSolverConfigsForScope SolverScopeHyp config.groups) $ \solver -> do
                        withPushLogContextOfflineSolver solver $ do
                            logDebug "running solver"
                            (result, elapsed) <- time $ runSolverWithLogging
                                solver.command
                                (executeSMTProofCheckOffline
                                    (Just config.timeout)
                                    solver.modelConfig
                                    check)
                            logOfflineSolverResult result elapsed
                            return $ mapSatResult
                                (SomeOfflineSolverAnsweredSat solver.commandName solver.modelConfig (OfflineSolversFailureIndexHyp i))
                                result

type OfflineSolversBackendForSingleCheck m a = SMTProofCheckWithFingerprint a -> m (Either (OfflineSolversFailureInfo ()) ())

runOfflineSoversBackendForSingleCheck
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OfflineSolversConfig -> OfflineSolversBackendForSingleCheck m a
runOfflineSoversBackendForSingleCheck config check = do
    withPushLogContext "offline" . withPushLogContextCheck check $
        fmap mapConclusion . forConcurrentlyUnliftIOE_ (offlineSolverConfigsForSingleCheck config.groups) $ \solver -> do
            withPushLogContextOfflineSolver solver $ do
                logDebug "running solver"
                (result, elapsed) <- time $ runSolverWithLogging
                    solver.command
                    (executeSMTProofCheckOffline
                        (Just config.timeout)
                        solver.modelConfig
                        check)
                logOfflineSolverResult result elapsed
                return $ mapSatResult
                    (SomeOfflineSolverAnsweredSat solver.commandName solver.modelConfig ())
                    result

type Conclusion a = Either a ()

pattern Conclusive :: a -> Either a ()
pattern Conclusive a = Left a

pattern Inconclusive :: Either a ()
pattern Inconclusive = Right ()

{-# COMPLETE Conclusive, Inconclusive #-}

mapConclusion :: Conclusion (Either (OfflineSolversFailureInfo i) ()) -> Either (OfflineSolversFailureInfo i) ()
mapConclusion = \case
    Conclusive conclusion -> conclusion
    Inconclusive -> Left AllOfflineSolversTimedOutOrAnsweredUnknown

mapSatResult :: OfflineSolversFailureInfo i -> Maybe SatResult -> Conclusion (Either (OfflineSolversFailureInfo i) ())
mapSatResult onSat = \case
    Just Sat -> Conclusive $
        Left onSat
    Just Unsat -> Conclusive $
        Right ()
    _ -> Inconclusive

withPushLogContextOfflineSolver :: MonadLoggerWithContext m => OfflineSolverConfig -> m a -> m a
withPushLogContextOfflineSolver solver = withPushLogContext ("solver " ++ solver.commandName ++ " " ++ memMode)
  where
    memMode = case solver.modelConfig.memoryMode of
        SolverMemoryModeWord8 -> "word8"
        SolverMemoryModeWord32 -> "word32"

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
