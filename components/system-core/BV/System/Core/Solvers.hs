module BV.System.Core.Solvers
    ( OfflineSolverBackend
    , OfflineSolverCommandName
    , OfflineSolverConfig (..)
    , OfflineSolverGroupConfig (..)
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
    , SolverGate
    , SolverScope (..)
    , SolversConfig (..)
    , localSolverBackend
    , prettySolverScope
    , runSolvers
    ) where

import BV.Core.Prelude
import BV.Logging
import BV.System.Core.Cache
import BV.System.Core.Report
import BV.System.Core.Solvers.Backend
import BV.System.Core.Solvers.Parallel
import BV.System.Core.Types
import BV.System.Core.Utils.Logging (withPushLogContextCheck)

import Control.Monad (filterM, (>=>))
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans (lift)
import Data.Foldable (for_)
import Data.List (genericIndex)
import GHC.Generics (Generic)
import Optics

-- TODO logging

data SolversConfig
  = SolversConfig
      { online :: Maybe OnlineSolverConfig
      , offline :: OfflineSolversConfig
      }
  deriving (Eq, Generic, Ord, Show)

runSolvers
    :: forall m. (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => SolverGate m
    -> SolverBackend m
    -> SolversConfig
    -> CheckSubgroup
    -> m CheckResult
runSolvers gate backend config subgroup = runExceptT $ do
    filterPipeline (deduplicateSubgroup subgroup)
  where
    filterPipeline =
        filterSubgroupUsingCache
            >=> filterSubgroupsUsingOnlineSolver gate backend config
            >=> checkUsingOfflineSolvers gate backend config

filterSubgroupUsingCache
    :: (MonadCache m, MonadLoggerWithContext m)
    => CheckSubgroup
    -> ExceptT CheckFailure m CheckSubgroup
filterSubgroupUsingCache = traverseOf #checks . filterM $ \(_i, check) -> withPushLogContextCheck check $ do
    cached <- queryCache check.fingerprint
    case cached of
        Nothing -> do
            return True
        Just AcceptableSatResultUnsat -> do
            logDebug $ "cache hit: unsat"
            return False
        Just AcceptableSatResultSat -> do
            logDebug $ "cache hit: sat"
            return False
            throwError $ CheckFailure
                { cause = SomeSolverAnsweredSat Cache
                , source = CheckFailureSourceCheck check
                }

filterSubgroupsUsingOnlineSolver
    :: (MonadLoggerWithContext m, MonadCache m)
    => SolverGate m
    -> SolverBackend m
    -> SolversConfig
    -> CheckSubgroup
    -> ExceptT CheckFailure m CheckSubgroup
filterSubgroupsUsingOnlineSolver gate backend config subgroup =
    case config.online of
        Just onlineConfig -> do
            result <- lift . gate 1 $ backend.online onlineConfig subgroup
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
    => SolverGate m
    -> SolverBackend m
    -> SolversConfig
    -> CheckSubgroup
    -> ExceptT CheckFailure m ()
checkUsingOfflineSolvers gate backend config subgroup =
    case subgroup.checks of
        [] -> do
            return ()
        [(_i, check)] -> do
            result <- lift . gate (numParallelSolversForSingleCheck config.offline) $
                runParellelOfflineSolversForSingleCheck
                    config.offline
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
            result <- lift . gate (numParallelSolvers config.offline) $
                runParellelOfflineSolvers
                    config.offline
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
