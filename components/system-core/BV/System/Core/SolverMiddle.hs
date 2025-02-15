{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}

module BV.System.Core.SolverMiddle
    ( SolverBackBackend (..)
    ) where

import BV.Core
import BV.Logging
import BV.SMTLIB2
import BV.SMTLIB2.Command
import BV.System.Core.Cache
import BV.System.Core.Report
import BV.System.Core.SolverBackBackend
import BV.System.Core.SolverBackend
import BV.System.Core.Utils.Logging
import BV.System.Core.WithFingerprints
import BV.System.Utils.UnliftIO.Async

import Control.Applicative (empty)
import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (filterM, unless, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (MonadError (throwError), liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.List (genericDrop, genericIndex, genericLength)
import GHC.Generics (Generic)
import Optics

data MiddleConfig
  = MiddleConfig
      { onlineSolverConfig :: Maybe OnlineSolverConfig
      , offlineSolversConfig :: OfflineSolversConfig
      }
  deriving (Eq, Generic, Ord, Show)

-- TODO cache successes, even in subgroup failure case
runSolvers
    :: forall m i. (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => (forall a. Integer -> m a -> m a) -> MiddleConfig -> SolverBackBackend m -> SMTProofCheckSubgroupWithFingerprints i -> m (SMTProofCheckResult i ())
runSolvers throttle config backend subgroup = runExceptT $ do
    uncached <- withPushLogContext "cache" $ keepUncached subgroup
    slow <- case config.onlineSolverConfig of
        Just onlineConfig -> do
            result <- lift $ throttle 1 $ runOnlineSolverBackend onlineConfig backend.online (void uncached)
            n <- case result of
                Right () -> return (genericLength uncached.inner.imps)
                Left failureInfo -> case failureInfo.reason of
                    OnlineSolverAnsweredSat -> throwError $ SMTProofCheckFailure
                        (SomeSolverAnsweredSat OnlineSolver)
                        (SMTProofCheckFailureSourceCheck (view #inner <$> splitSMTProofCheckMetaWithFingerprint (uncached.inner.imps `genericIndex` failureInfo.index).meta))
                    _ -> return failureInfo.index
            return $ uncached & #inner % #imps %~ genericDrop n
        Nothing -> return uncached
    case ungroupSMTProofCheckGroup slow.inner of
        [] -> do
            return ()
        [check] -> do
            result <- lift $ throttle (numParallelSolversForSingleCheck config.offlineSolversConfig) $ runOfflineSolverBackendForSingleCheck
                config.offlineSolversConfig
                backend.offlineSingle
                (check & #imp % #meta % #inner % #inner .~ ())
            liftEither $ flip first result $ \failureInfo ->
                let f cause = SMTProofCheckFailure
                        { cause
                        , source = SMTProofCheckFailureSourceCheck (splitSMTProofCheckMetaWithFingerprint (check.imp.meta & #inner %~ view #inner))
                        }
                 in f $ case failureInfo of
                        OfflineSolversFailureInfoForSingleCheckSomeAnsweredSat solverCmdName modelConfig ->
                            SomeSolverAnsweredSat (OfflineSolver solverCmdName modelConfig)
                        OfflineSolversFailureInfoForSingleCheckAllTimedOutOrAnsweredUnknown ->
                            AllSolversTimedOutOrAnsweredUnknown
        _ -> do
            result <- lift $ throttle (numParallelSolvers config.offlineSolversConfig) $ runOfflineSolverBackend
                config.offlineSolversConfig
                backend.offline
                backend.offlineSingle
                (void slow)
            liftEither $ flip first result $ \failureInfo ->
                let f cause = SMTProofCheckFailure
                        { cause
                        , source = SMTProofCheckFailureSourceCheckSubgroup
                            slow.groupFingerprint
                            (map (splitSMTProofCheckMetaWithFingerprint . (#inner %~ view #inner) . (.meta)) slow.inner.imps)
                        }
                 in f $ case failureInfo.cause of
                        SomeOfflineSolverAnsweredSat loc solverCmdName modelConfig ->
                            SomeSolverAnsweredSat (OfflineSolver solverCmdName modelConfig)
                        AllOfflineSolversTimedOutOrAnsweredUnknown ->
                            AllSolversTimedOutOrAnsweredUnknown

keepUncached
    :: (MonadLoggerWithContext m, MonadCache m, MonadError (SMTProofCheckFailure i) m)
    => SMTProofCheckSubgroupWithFingerprints i
    -> m (SMTProofCheckSubgroupWithFingerprints i)
keepUncached group = forOf (#inner % #imps) group $ \imps ->
    flip filterM imps (\imp -> do
        let fingerprint = imp.meta.fingerprint
        withPushLogContextCheckFingerprint fingerprint $ do
            cached <- queryCacheUsingFingerprint fingerprint
            case cached of
                Nothing -> return True
                Just AcceptableSatResultUnsat -> return False
                Just AcceptableSatResultSat -> throwError $ SMTProofCheckFailure
                    (SomeSolverAnsweredSat Cache)
                    (SMTProofCheckFailureSourceCheck (splitSMTProofCheckMetaWithFingerprint (view #inner <$> imp.meta))))
