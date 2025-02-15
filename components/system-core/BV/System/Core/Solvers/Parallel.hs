{-# LANGUAGE PatternSynonyms #-}

module BV.System.Core.Solvers.Parallel
    ( OfflineSolverBackend
    , OfflineSolverBackendForSingleCheck
    , OfflineSolverCommandName
    , OfflineSolverConfig (..)
    , OfflineSolverGroupConfig (..)
    , OfflineSolversConfig (..)
    , OfflineSolversFailureCause (..)
    , OfflineSolversFailureCauseLocation (..)
    , OfflineSolversFailureInfo (..)
    , OfflineSolversFailureInfoForSingleCheck (..)
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

import BV.Core
import BV.Logging
import BV.SMTLIB2
import BV.SMTLIB2.Command
import BV.System.Core.Solvers.Backend
import BV.System.Core.Utils.Logging
import BV.System.Core.WithFingerprints
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
import Data.List (genericLength)
import GHC.Generics (Generic)

--

type OnlineSolverBackend a m
    = OnlineSolverBackBackend a m
    -> SMTProofCheckSubgroupWithFingerprints a
    -> m (Either OnlineSolverFailureInfo ())

runOnlineSolverBackend
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverConfig -> OnlineSolverBackend a m
runOnlineSolverBackend config backend subgroup = do
    backend config subgroup

--

data OfflineSolversConfig
  = OfflineSolversConfig
      { groups :: [OfflineSolverGroupConfig]
      , timeout :: SolverTimeout
      }
  deriving (Eq, Generic, Ord, Show)

data OfflineSolverGroupConfig
  = OfflineSolverGroupConfig
      { commandName :: OfflineSolverCommandName
      , command :: SolverCommand
      , scopes :: [SolverScope]
      , modelConfigs :: [ModelConfig]
      }
  deriving (Eq, Generic, Ord, Show)

data SolverScope
  = SolverScopeHyp
  | SolverScopeAll
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

prettySolverScope :: SolverScope -> String
prettySolverScope = \case
    SolverScopeAll -> "all"
    SolverScopeHyp -> "hyp"

offlineSolverConfigsForScope :: SolverScope -> OfflineSolversConfig -> [OfflineSolverConfig]
offlineSolverConfigsForScope scope config = flip concatMap config.groups $
    \OfflineSolverGroupConfig { commandName, command, scopes, modelConfigs } -> do
        unless (scope `elem` scopes) empty
        modelConfig <- modelConfigs
        return $ OfflineSolverConfig
            { commandName
            , command
            , modelConfig
            , timeout = config.timeout
            }

offlineSolverConfigsForSingleCheck :: OfflineSolversConfig -> [OfflineSolverConfig]
offlineSolverConfigsForSingleCheck config = flip concatMap config.groups $
    \OfflineSolverGroupConfig { commandName, command, scopes, modelConfigs } -> do
        when (null scopes) empty
        modelConfig <- modelConfigs
        return $ OfflineSolverConfig
            { commandName
            , command
            , modelConfig
            , timeout = config.timeout
            }

numParallelSolvers :: OfflineSolversConfig -> Integer
numParallelSolvers config = sum
    [ genericLength (offlineSolverConfigsForScope scope config)
    | scope <- [minBound..maxBound]
    ]

numParallelSolversForSingleCheck :: OfflineSolversConfig -> Integer
numParallelSolversForSingleCheck config = genericLength (offlineSolverConfigsForSingleCheck config)

--

type OfflineSolverBackend a m
    = OfflineSolverCheckSubgroupBackBackend a m
    -> OfflineSolverCheckBackBackend (SubgroupElementMeta a) m
    -> SMTProofCheckSubgroupWithFingerprints a
    -> m (Either OfflineSolversFailureInfo ())

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

-- TODO

-- data OfflineSolversFailureInfo
--   = OfflineSolversFailureInfo
--       { numSuccessfulHyps :: Integer
--       , cause :: OfflineSolversFailureCause
--       , location :: OfflineSolversFailureMostSpecificLocation
--       }
--   deriving (Eq, Generic, Ord, Show)

-- data OfflineSolversFailureCause
--   = SomeOfflineSolverAnsweredSat OfflineSolverCommandName ModelConfig
--   | AllOfflineSolversTimedOutOrAnsweredUnknown
--   deriving (Eq, Generic, Ord, Show)

-- data OfflineSolversFailureMostSpecificLocation
--   = OfflineSolversFailureIndexAll
--   | OfflineSolversFailureIndexHyp
--       { index :: Integer
--       }
--   deriving (Eq, Generic, Ord, Show)

runOfflineSolverBackend
    :: forall m a. (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OfflineSolversConfig -> OfflineSolverBackend a m
runOfflineSolverBackend config checkSubgroupBackend checkBackend subgroup = do
    withPushLogContext "offline" . withPushLogContextCheckSubgroup subgroup $ do
        numSuccessfulHypsVar <- liftIO $ newTVarIO 0
        let allStrategy :: m (ConclusionResult (Either OfflineSolversFailureCause ()))
            allStrategy = do
                withPushLogContext "all" $ do
                    forConcurrentlyUnliftIOE_ (offlineSolverConfigsForScope SolverScopeAll config) $ \solver -> do
                        withPushLogContextOfflineSolver solver $ do
                            satResult <- checkSubgroupBackend solver subgroup
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
                        for_ (zip [0..] (ungroupSMTProofCheckSubgroupWithFingerprints subgroup)) $ \(i, check) -> do
                            conclusionResult <- lift . forConcurrentlyUnliftIOE_ (offlineSolverConfigsForScope SolverScopeHyp config) $ \solver -> do
                                withPushLogContextOfflineSolver solver $ do
                                    satResult <- checkBackend solver check
                                    return $ satResultToConclusionResult
                                        (SomeOfflineSolverAnsweredSat
                                            (OfflineSolversFailureIndexHyp { index = i })
                                            solver.commandName
                                            solver.modelConfig)
                                        satResult
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

type OfflineSolverBackendForSingleCheck a m
    = OfflineSolverCheckBackBackend a m
    -> SMTProofCheckWithFingerprint a
    -> m (Either OfflineSolversFailureInfoForSingleCheck ())

data OfflineSolversFailureInfoForSingleCheck
  = OfflineSolversFailureInfoForSingleCheckSomeAnsweredSat OfflineSolverCommandName ModelConfig
  | OfflineSolversFailureInfoForSingleCheckAllTimedOutOrAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

runOfflineSolverBackendForSingleCheck
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OfflineSolversConfig -> OfflineSolverBackendForSingleCheck a m
runOfflineSolverBackendForSingleCheck config backend check = do
    withPushLogContext "offline" . withPushLogContextCheck check $ do
        conclusionResult <- forConcurrentlyUnliftIOE_ (offlineSolverConfigsForSingleCheck config) $ \solver -> do
            withPushLogContextOfflineSolver solver $ do
                satResult <- backend solver check
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
