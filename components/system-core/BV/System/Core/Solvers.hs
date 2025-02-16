{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}

module BV.System.Core.Solvers
    ( OfflineSolverBackend
    , OfflineSolverCommandName
    , OfflineSolverConfig (..)
    , OfflineSolverSingleBackend
    , OfflineSolversConfig (..)
    , OfflineSolversFailureCause (..)
    , OfflineSolversFailureCauseLocation (..)
    , OfflineSolversFailureInfo (..)
    , OfflineSolversFailureInfoForSingleCheck (..)
    , OnlineSolverBackend
    , OnlineSolverConfig (..)
    , SolverBackend (..)
    , SolverCommand (..)
    , SolverScope (..)
    , SolversConfig (..)
    , prettySolverScope
    , runSolvers
    ) where

import BV.Core
import BV.Logging
import BV.System.Core.Cache
import BV.System.Core.Report
import BV.System.Core.Solvers.Backend
import BV.System.Core.Solvers.Parallel
import BV.System.Core.Types
import BV.System.Core.Utils.Logging

import Control.Monad (filterM, (>=>))
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans (lift)
import Data.Foldable (for_)
import Data.List (genericIndex)
import GHC.Generics (Generic)
import Optics

data SolversConfig
  = SolversConfig
      { onlineSolverConfig :: Maybe OnlineSolverConfig
      , offlineSolversConfig :: OfflineSolversConfig
      }
  deriving (Eq, Generic, Ord, Show)

runSolvers
    :: forall m. (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => (forall a. Integer -> m a -> m a)
    -> SolverBackend m
    -> SolversConfig
    -> CheckSubgroup
    -> m CheckResult
runSolvers throttle backend config subgroup = runExceptT $ do
    filterPipeline subgroup
  where
    filterPipeline =
        filterSubgroupUsingCache
            >=> filterSubgroupsUsingOnlineSolver throttle backend config
            >=> checkUsingOfflineSolvers throttle backend config

filterSubgroupUsingCache
    :: (MonadLoggerWithContext m, MonadCache m)
    => CheckSubgroup
    -> ExceptT CheckFailure m CheckSubgroup
filterSubgroupUsingCache = traverseOf #checks . filterM $ \(_i, check) -> do
    withPushLogContext "cache" $
        withPushLogContextCheck check $ do
            cached <- queryCache check.fingerprint
            case cached of
                Nothing -> return True
                Just AcceptableSatResultUnsat -> return False
                Just AcceptableSatResultSat -> throwError $ CheckFailure
                    { cause = SomeSolverAnsweredSat Cache
                    , source = CheckFailureSourceCheck check
                    }

filterSubgroupsUsingOnlineSolver
    :: (MonadLoggerWithContext m, MonadCache m)
    => (forall a. Integer -> m a -> m a)
    -> SolverBackend m
    -> SolversConfig
    -> CheckSubgroup
    -> ExceptT CheckFailure m CheckSubgroup
filterSubgroupsUsingOnlineSolver throttle backend config subgroup =
    case config.onlineSolverConfig of
        Just onlineConfig -> do
            result <- lift . throttle 1 $ backend.online onlineConfig subgroup
            let (unsat, rest) = case result of
                    Right () -> (subgroup, Right (takeEmptySubgroup subgroup))
                    Left failureInfo ->
                        let (unsat', notYetChecked) = splitSubgroupAt failureInfo.index subgroup
                            rest' = case failureInfo.reason of
                                OnlineSolverAnsweredSat ->
                                    let (_i, check) = subgroup.checks `genericIndex` failureInfo.index
                                     in Left check

                                _ -> Right notYetChecked
                         in (unsat', rest')
            for_ unsat.checks $ \(_i, check) -> do
                updateCache AcceptableSatResultUnsat check.fingerprint
            case rest of
                Right notYetChecked -> do
                    return notYetChecked
                Left sat -> do
                    updateCache AcceptableSatResultSat sat.fingerprint
                    throwError $ CheckFailure
                        { cause = SomeSolverAnsweredSat OnlineSolver
                        , source = CheckFailureSourceCheck sat
                        }
        Nothing -> return subgroup

checkUsingOfflineSolvers
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => (forall a. Integer -> m a -> m a)
    -> SolverBackend m
    -> SolversConfig
    -> CheckSubgroup
    -> ExceptT CheckFailure m ()
checkUsingOfflineSolvers throttle backend config subgroup =
    case subgroup.checks of
        [] -> do
            return ()
        [(_i, check)] -> do
            result <- lift . throttle (numParallelSolversForSingleCheck config.offlineSolversConfig) $
                runParellelOfflineSolversForSingleCheck
                    config.offlineSolversConfig
                    backend.offlineSingle
                    check
            case result of
                Right () -> do
                    updateCache AcceptableSatResultUnsat check.fingerprint
                Left failureInfo -> do
                    cause <- case failureInfo of
                        OfflineSolversFailureInfoForSingleCheckSomeAnsweredSat solverCmdName modelConfig -> do
                            updateCache AcceptableSatResultSat check.fingerprint
                            return $ SomeSolverAnsweredSat (OfflineSolver solverCmdName modelConfig)
                        OfflineSolversFailureInfoForSingleCheckAllTimedOutOrAnsweredUnknown -> do
                            return AllSolversTimedOutOrAnsweredUnknown
                    throwError $ CheckFailure
                        { cause
                        , source = CheckFailureSourceCheck check
                        }
        _ -> do
            result <- lift . throttle (numParallelSolvers config.offlineSolversConfig) $
                runParellelOfflineSolvers
                    config.offlineSolversConfig
                    backend.offline
                    backend.offlineSingle
                    subgroup
            case result of
                Right () -> do
                    for_ subgroup.checks $ \(_i, check) -> do
                        updateCache AcceptableSatResultUnsat check.fingerprint
                Left failureInfo -> do
                    let (unsat, _) = splitSubgroupAt failureInfo.numSuccessfulChecks subgroup
                    for_ unsat.checks $ \(_i, check) -> do
                        updateCache AcceptableSatResultUnsat check.fingerprint
                    cause <- case failureInfo.cause of
                        SomeOfflineSolverAnsweredSat satAnswser -> do
                            case satAnswser.location of
                                OfflineSolversFailureCauseLocationHyp hypIndex -> do
                                    let check = getHypAtIndex hypIndex subgroup
                                    updateCache AcceptableSatResultUnsat check.fingerprint
                                _ -> do
                                    return ()
                            return $ SomeSolverAnsweredSat
                                (OfflineSolver
                                    satAnswser.offlineSolverCommandName
                                    satAnswser.modelConfig)
                        AllOfflineSolversTimedOutOrAnsweredUnknown hypIndex -> do
                            let check = getHypAtIndex hypIndex subgroup
                            updateCache AcceptableSatResultUnsat check.fingerprint
                            return AllSolversTimedOutOrAnsweredUnknown
                    throwError $ CheckFailure
                        { cause
                        , source = CheckFailureSourceCheckSubgroup subgroup
                        }
