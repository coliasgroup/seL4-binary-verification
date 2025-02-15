{-# LANGUAGE PatternSynonyms #-}

module BV.System.Core.SolverBackend
    ( OfflineSolverBackend
    , OfflineSolverBackendForSingleCheck
    , OfflineSolverCommandName
    , OfflineSolverConfig (..)
    , OfflineSolverGroupConfig (..)
    , OfflineSolversConfig (..)
    , OfflineSolversFailureCause (..)
    , OfflineSolversFailureCauseLocation (..)
    , OfflineSolversFailureInfo (..)
    , OnlineSolverBackend
    , OnlineSolverConfig (..)
    , SolverScope (..)
    , numParallelSolvers
    , numParallelSolversForSingleCheck
    , prettySolverScope
    , runOfflineSolverBackend
    , runOfflineSolverBackendForSingleCheck
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
import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
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

type OnlineSolverBackend a m = SMTProofCheckSubgroupWithFingerprints a -> m (Either OnlineSolverFailureInfo ())

runOnlineSolverBackend
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverConfig -> OnlineSolverBackend a m
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

type OfflineSolverBackend a m = SMTProofCheckSubgroupWithFingerprints a -> m (Either OfflineSolversFailureInfo ())

data OfflineSolversFailureInfo
  = OfflineSolversFailureInfo
      { numSuccessfulHyps :: Integer
      , cause :: OfflineSolversFailureCause
      }
  deriving (Eq, Generic, Ord, Show)

data OfflineSolversFailureCause
  = SomeOfflineSolverAnsweredSat OfflineSolversFailureCauseLocation OfflineSolverCommandName ModelConfig
  | AllOfflineSolversTimedOutOrAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

data OfflineSolversFailureCauseLocation
  = OfflineSolversFailureIndexAll
  | OfflineSolversFailureIndexHyp
      { index :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

runOfflineSolverBackend
    :: forall m a. (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OfflineSolversConfig -> OfflineSolverBackend a m
runOfflineSolverBackend config subgroup = do
    withPushLogContext "offline" . withPushLogContextCheckSubgroup subgroup $ do
        numSuccessfulHypsVar <- liftIO $ newTVarIO 0
        let allStrategy :: m (ConclusionResult (Either OfflineSolversFailureCause ()))
            allStrategy = do
                withPushLogContext "all" $ do
                    forConcurrentlyUnliftIOE_ (offlineSolverConfigsForScope SolverScopeAll config.groups) $ \solver -> do
                        withPushLogContextOfflineSolver solver $ do
                            logDebug "running solver"
                            (satResult, elapsed) <- time $ runSolverWithLogging
                                solver.command
                                (executeSMTProofCheckGroupOffline
                                    (Just config.timeout)
                                    solver.modelConfig
                                    subgroup.inner)
                            logOfflineSolverSatResult satResult elapsed
                            return $ satResultToConclusionResult
                                (SomeOfflineSolverAnsweredSat
                                    OfflineSolversFailureIndexAll
                                    solver.commandName
                                    solver.modelConfig)
                                satResult
        let hypStrategy :: m (ConclusionResult (Either OfflineSolversFailureCause ()))
            hypStrategy = do
                withPushLogContext "hyp" $ do
                    result <- runExceptT $ do
                        for_ (zip [0..] (ungroupSMTProofCheckGroup subgroup.inner)) $ \(i, check) -> do
                            conclusionResult <- lift . forConcurrentlyUnliftIOE_ (offlineSolverConfigsForScope SolverScopeHyp config.groups) $ \solver -> do
                                withPushLogContextOfflineSolver solver $ do
                                    logDebug "running solver"
                                    (result, elapsed) <- time $ runSolverWithLogging
                                        solver.command
                                        (executeSMTProofCheckOffline
                                            (Just config.timeout)
                                            solver.modelConfig
                                            check)
                                    logOfflineSolverSatResult result elapsed
                                    return $ satResultToConclusionResult
                                        (SomeOfflineSolverAnsweredSat
                                            (OfflineSolversFailureIndexHyp { index = i })
                                            solver.commandName
                                            solver.modelConfig)
                                        result
                            liftEither $ flattenConclusion conclusionResult
                            liftIO . atomically $ writeTVar numSuccessfulHypsVar i
                    return $ case result of
                        Right () -> Conclusive (Right ())
                        Left AllOfflineSolversTimedOutOrAnsweredUnknown -> Inconclusive
                        Left otherFailure -> Conclusive (Left otherFailure)
        conclusionResult <- concurrentlyUnliftIOE_ allStrategy hypStrategy
        numSuccessfulHyps <- liftIO $ readTVarIO numSuccessfulHypsVar
        return . first (OfflineSolversFailureInfo numSuccessfulHyps) $ flattenConclusion conclusionResult

flattenConclusion :: ConclusionResult (Either OfflineSolversFailureCause ()) -> Either OfflineSolversFailureCause ()
flattenConclusion = \case
    Conclusive conclusion -> conclusion
    Inconclusive -> Left AllOfflineSolversTimedOutOrAnsweredUnknown

--

type OfflineSolverBackendForSingleCheck a m = SMTProofCheckWithFingerprint a -> m (Either OfflineSolversFailureInfoForSingleCheck ())

data OfflineSolversFailureInfoForSingleCheck
  = OfflineSolversFailureInfoForSingleCheckSomeAnsweredSat OfflineSolverCommandName ModelConfig
  | OfflineSolversFailureInfoForSingleCheckAllTimedOutOrAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

runOfflineSolverBackendForSingleCheck
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OfflineSolversConfig -> OfflineSolverBackendForSingleCheck a m
runOfflineSolverBackendForSingleCheck config check = do
    withPushLogContext "offline" . withPushLogContextCheck check $ do
        conclusionResult <- forConcurrentlyUnliftIOE_ (offlineSolverConfigsForSingleCheck config.groups) $ \solver -> do
            withPushLogContextOfflineSolver solver $ do
                logDebug "running solver"
                (satResult, elapsed) <- time $ runSolverWithLogging
                    solver.command
                    (executeSMTProofCheckOffline
                        (Just config.timeout)
                        solver.modelConfig
                        check)
                logOfflineSolverSatResult satResult elapsed
                return $ satResultToConclusionResult
                    (OfflineSolversFailureInfoForSingleCheckSomeAnsweredSat
                        solver.commandName
                        solver.modelConfig)
                    satResult
        return $ case conclusionResult of
            Conclusive conclusion -> conclusion
            Inconclusive -> Left OfflineSolversFailureInfoForSingleCheckAllTimedOutOrAnsweredUnknown

type ConclusionResult a = Either a ()

pattern Conclusive :: a -> ConclusionResult a
pattern Conclusive a = Left a

pattern Inconclusive :: ConclusionResult a
pattern Inconclusive = Right ()

{-# COMPLETE Conclusive, Inconclusive #-}

satResultToConclusionResult :: c -> Maybe SatResult -> ConclusionResult (Either c ())
satResultToConclusionResult onSat = \case
    Just Sat -> Conclusive (Left onSat)
    Just Unsat -> Conclusive (Right ())
    _ -> Inconclusive

--

withPushLogContextOfflineSolver :: MonadLoggerWithContext m => OfflineSolverConfig -> m a -> m a
withPushLogContextOfflineSolver solver = withPushLogContext ("solver " ++ solver.commandName ++ " " ++ memMode)
  where
    memMode = case solver.modelConfig.memoryMode of
        SolverMemoryModeWord8 -> "word8"
        SolverMemoryModeWord32 -> "word32"

logOfflineSolverSatResult :: MonadLoggerWithContext m => Maybe SatResult -> Elapsed -> m ()
logOfflineSolverSatResult result elapsed = do
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
