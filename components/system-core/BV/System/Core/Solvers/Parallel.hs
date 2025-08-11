{-# LANGUAGE PatternSynonyms #-}

module BV.System.Core.Solvers.Parallel
    ( OfflineSolverGroupConfig (..)
    , OfflineSolverSatAnswer (..)
    , OfflineSolversConfig (..)
    , OfflineSolversFailureCause (..)
    , OfflineSolversFailureCauseLocation (..)
    , OfflineSolversFailureInfo (..)
    , OfflineSolversFailureInfoForSingleCheck (..)
    , SolverScope (..)
    , getHypAtIndex
    , numParallelSolvers
    , numParallelSolversForSingleCheck
    , prettySolverScope
    , runParellelOfflineSolvers
    , runParellelOfflineSolversForSingleCheck
    ) where

import BV.Core.Prelude
import BV.Logging
import BV.SMTLIB2
import BV.SMTLIB2.Command
import BV.System.Core.Solvers.Backend
import BV.System.Core.Types
import BV.System.Core.Utils.Logging
import BV.System.Utils.UnliftIO.Async

import Control.Applicative (empty)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import Data.Foldable (for_, toList)
import Data.List (genericLength)
import GHC.Generics (Generic)

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

data OfflineSolversFailureInfo
  = OfflineSolversFailureInfo
      { numSuccessfulChecks :: Integer
      , cause :: OfflineSolversFailureCause
      }
  deriving (Eq, Generic, Ord, Show)

data OfflineSolversFailureCause
  = SomeOfflineSolverAnsweredSat OfflineSolverSatAnswer
  | AllOfflineSolversTimedOutOrAnsweredUnknown OfflineSolversFailureHypIndex
  deriving (Eq, Generic, Ord, Show)

data OfflineSolverSatAnswer
  = OfflineSolverSatAnswer
      { location :: OfflineSolversFailureCauseLocation
      , offlineSolverCommandName :: OfflineSolverCommandName
      , modelConfig :: ModelConfig
      }
  deriving (Eq, Generic, Ord, Show)

data OfflineSolversFailureCauseLocation
  = OfflineSolversFailureCauseLocationAll
  | OfflineSolversFailureCauseLocationHyp
      { hypIndex :: OfflineSolversFailureHypIndex
      }
  deriving (Eq, Generic, Ord, Show)

newtype OfflineSolversFailureHypIndex
  = OfflineSolversFailureHypIndex { unwrap :: Integer }
  deriving (Eq, Generic, Ord, Show)

getHypAtIndex :: OfflineSolversFailureHypIndex -> CheckSubgroup -> Check
getHypAtIndex i subgroup = subgroup `indexSubgroup` i.unwrap

runParellelOfflineSolvers
    :: forall m. (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OfflineSolversConfig
    -> OfflineSolverBackend m
    -> OfflineSolverSingleBackend m
    -> CheckSubgroup
    -> m (Either OfflineSolversFailureInfo ())
runParellelOfflineSolvers config checkSubgroupBackend checkBackend subgroup = do
    withPushLogContext "offline" . withPushLogContextCheckGroup subgroup.group $ do
        numSuccessfulChecksVar <- liftIO $ newTVarIO 0
        let allStrategy :: m (ConclusionResult (Either OfflineSolverSatAnswer ()) ())
            allStrategy = do
                withPushLogContext "all" $ do
                    forConcurrentlyUnliftIOE_ (offlineSolverConfigsForScope SolverScopeAll config) $ \solver -> do
                        withPushLogContextOfflineSolver solver $ do
                            satResult <- checkSubgroupBackend solver subgroup
                            return $ satResultToConclusionResult
                                (OfflineSolverSatAnswer
                                    { location = OfflineSolversFailureCauseLocationAll
                                    , offlineSolverCommandName = solver.commandName
                                    , modelConfig = solver.modelConfig
                                    })
                                satResult
        let hypStrategy :: m (ConclusionResult (Either OfflineSolverSatAnswer ()) OfflineSolversFailureHypIndex)
            hypStrategy = do
                withPushLogContext "hyp" $ do
                    result <- runExceptT $ do
                        for_ (zip (map OfflineSolversFailureHypIndex [0..]) (toList subgroup.checks)) $ \(hypIndex, check) -> do
                            conclusionResult <- lift . forConcurrentlyUnliftIOE_ (offlineSolverConfigsForScope SolverScopeHyp config) $ \solver -> do
                                withPushLogContextOfflineSolver solver $ do
                                    satResult <- checkBackend solver check
                                    return $ satResultToConclusionResult
                                        (OfflineSolverSatAnswer
                                            { location = OfflineSolversFailureCauseLocationHyp hypIndex
                                            , offlineSolverCommandName = solver.commandName
                                            , modelConfig = solver.modelConfig
                                            })
                                        satResult
                            liftEither $ flattenConclusionResult (const hypIndex) conclusionResult
                            liftIO . atomically $ modifyTVar' numSuccessfulChecksVar (+ 1)
                    return $ case result of
                        Right () -> Left (Right ())
                        Left (SomeOfflineSolverAnsweredSat satAnswer) -> Left (Left satAnswer)
                        Left (AllOfflineSolversTimedOutOrAnsweredUnknown hypIndex) -> Right hypIndex
        conclusionResult :: Either (Either OfflineSolverSatAnswer ()) OfflineSolversFailureHypIndex <- runConcurrentlyUnliftIOE $
            makeConcurrentlyUnliftIOE allStrategy *> makeConcurrentlyUnliftIOE hypStrategy
        numSuccessfulChecks <- liftIO $ readTVarIO numSuccessfulChecksVar
        return . first (OfflineSolversFailureInfo numSuccessfulChecks) $ flattenConclusionResult id conclusionResult

flattenConclusionResult :: (a -> OfflineSolversFailureHypIndex) -> ConclusionResult (Either OfflineSolverSatAnswer ()) a -> Either OfflineSolversFailureCause ()
flattenConclusionResult f = \case
    Conclusive (Left satAnswer) -> Left (SomeOfflineSolverAnsweredSat satAnswer)
    Conclusive (Right ()) -> Right ()
    Inconclusive a -> Left (AllOfflineSolversTimedOutOrAnsweredUnknown (f a))

--

data OfflineSolversFailureInfoForSingleCheck
  = OfflineSolversFailureInfoForSingleCheckSomeAnsweredSat OfflineSolverCommandName ModelConfig
  | OfflineSolversFailureInfoForSingleCheckAllTimedOutOrAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

runParellelOfflineSolversForSingleCheck
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OfflineSolversConfig
    -> OfflineSolverSingleBackend m
    -> Check
    -> m (Either OfflineSolversFailureInfoForSingleCheck ())
runParellelOfflineSolversForSingleCheck config backend check = do
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
            Inconclusive () -> Left OfflineSolversFailureInfoForSingleCheckAllTimedOutOrAnsweredUnknown

type ConclusionResult c a = Either c a

pattern Conclusive :: c -> ConclusionResult c a
pattern Conclusive c = Left c

pattern Inconclusive :: a -> ConclusionResult c a
pattern Inconclusive a = Right a

{-# COMPLETE Conclusive, Inconclusive #-}

satResultToConclusionResult :: c -> Maybe SatResult -> ConclusionResult (Either c ()) ()
satResultToConclusionResult onSat = \case
    Just Sat -> Conclusive (Left onSat)
    Just Unsat -> Conclusive (Right ())
    _ -> Inconclusive ()

--

withPushLogContextOfflineSolver :: MonadLoggerWithContext m => OfflineSolverConfig -> m a -> m a
withPushLogContextOfflineSolver solver =
    withPushLogContext ("solver " ++ solver.commandName ++ " " ++ prettyModelConfig solver.modelConfig)
