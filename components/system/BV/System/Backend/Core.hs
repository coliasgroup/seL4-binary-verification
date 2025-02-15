{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Use null" #-}

module BV.System.Backend.Core
    ( BackendCoreConfig (..)
    , backendCore
    , backendCoreSingleCheck
    ) where

import BV.Core.ExecuteSMTProofChecks
import BV.Core.Types
import BV.Logging
import BV.SMTLIB2
import BV.SMTLIB2.Command
import BV.SMTLIB2.Process
import BV.System.Core.Cache
import BV.System.Core.Utils.Logging
import BV.System.Core.WithFingerprints
import BV.System.Frontend
import BV.System.SolversConfig
import BV.System.Utils.Stopwatch
import BV.System.Utils.Throttle
import BV.System.Utils.UnliftIO.Async
import BV.System.Utils.UnliftIO.Throttle

import Control.Monad (filterM, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT (ExceptT), MonadError, liftEither,
                             mapExceptT, runExceptT, throwError)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans (lift)
import Data.Foldable (for_)
import Data.List (genericDrop, genericIndex, genericLength)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Optics
import System.Process (proc)
import Text.Printf (printf)

data BackendCoreConfig
  = BackendCoreConfig
      { solversConfig :: SolversConfig
      }
  deriving (Eq, Generic, Ord, Show)

offlineOnly :: Bool
offlineOnly = False
-- offlineOnly = True

backendCore
    :: forall m i. (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => BackendCoreConfig -> Throttle -> SMTProofCheckGroupWithFingerprints i -> m (SMTProofCheckResult i ())
backendCore config throttle group = runExceptT $ do
    uncached <- withPushLogContext "cache" $ keepUncached group
    slow <- case config.solversConfig.online of
        Just onlineConfig -> backendCoreOnline onlineConfig throttle uncached
        Nothing -> return uncached
    backendCoreOffline config.solversConfig.offline throttle slow

backendCoreOnline
    :: forall m i. (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => OnlineSolverConfig -> Throttle -> SMTProofCheckGroupWithFingerprints i -> ExceptT (SMTProofCheckError i) m (SMTProofCheckGroupWithFingerprints i)
backendCoreOnline config throttle group = mapExceptT (withPushLogContext "online") $ do
    (exit, _elapsed) <-
        lift $ withThrottleUnliftIO throttle defaultPriority (Units 1) $ runSolver'
            config.command
            (executeSMTProofCheckGroupOnline
                (Just config.timeout)
                config.modelConfig
                group.inner)
    let numCompleted = case exit of
            Right _ -> genericLength group.inner.imps
            Left abort -> abort.index
    for_ [0..(numCompleted - 1)] $ \i -> do
        let check = checkAt i
        withPushLogContextCheck check $ do
            logDebug "answered unsat"
        updateCache AcceptableSatResultUnsat check
    case exit of
        Right () -> return ()
        Left abort -> do
            let check = checkAt abort.index
            withPushLogContextCheck check $ do
                case abort.reason of
                    OnlineSolverTimedOut -> do
                        logDebug "timeout"
                    OnlineSolverAnsweredSat -> do
                        logDebug "answered sat"
                        updateCache AcceptableSatResultSat check
                        throwError $ SMTProofCheckError
                            (SomeSolverAnsweredSat OnlineSolver)
                            (SMTProofCheckSourceCheck check.imp.meta)
                    OnlineSolverAnsweredUnknown reason -> do
                        logDebug $ "answered unknown: " ++ showSExpr reason
    let remaining = group & #inner % #imps %~ genericDrop numCompleted
    return remaining
  where
    checkAt i = ungroupSMTProofCheckGroup group.inner `genericIndex` i

-- TODO return units when they become available with more granularity
backendCoreOffline
    :: forall m i. (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => OfflineSolversConfig -> Throttle -> SMTProofCheckGroupWithFingerprints i -> ExceptT (SMTProofCheckError i) m ()
backendCoreOffline config throttle group =
    mapExceptT (withPushLogContext "offline") $ do
        when doAny . mapExceptT (withThrottleUnliftIO throttle defaultPriority unitsTotal) $ do
            conclusion <- lift . runConclusionT $ concurrentlyUnliftIOC_ concAll concHyp
            case conclusion of
                Nothing -> throwError $ SMTProofCheckError AllSolversTimedOutOrAnsweredUnknown allLocs
                Just c -> liftEither c
  where
    doAny = length group.inner.imps > 0
    doAll = length group.inner > 1 || numOfflineSolverConfigsForScope SolverScopeHyp config.groups == 0
    unitsTotal = unitsAll + unitsHyp
    (unitsAll, concAll) =
        if doAll
        then backendCoreOfflineHyp config group
        else (0, return ())
    (unitsHyp, concHyp) = backendCoreOfflineAll config group
    allLocs = SMTProofCheckSourceCheckSubgroup group.fingerprint (map (.meta) group.inner.imps)

-- TODO return units when they become available with more granularity
backendCoreOfflineAll
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => OfflineSolversConfig -> SMTProofCheckGroupWithFingerprints i -> (Units, ConclusionT (SMTProofCheckResult i ()) m ())
backendCoreOfflineAll config group = (units, concM)
  where
    units = Units (numOfflineSolverConfigsForScope SolverScopeAll config.groups)
    allLocs = SMTProofCheckSourceCheckSubgroup group.fingerprint (map (.meta) group.inner.imps)
    concM = withPushLogContext "all" . runConcurrentlyUnliftIOC $ do
        for_ (offlineSolverConfigsForScope SolverScopeAll config.groups) $ \solver ->
            makeConcurrentlyUnliftIOC . withPushLogContextOfflineSolver solver $ do
                checkSatOutcome <- lift $ runSolver''
                    solver.command
                    (executeSMTProofCheckGroupOffline
                        (Just config.timeout)
                        solver.config
                        group.inner)
                case checkSatOutcome of
                    Just Sat -> do
                        for_ (ungroupSMTProofCheckGroup group.inner) (updateCache AcceptableSatResultSat)
                        throwConclusion . Left $ SMTProofCheckError
                            (SomeSolverAnsweredSat (OfflineSolver solver.commandName solver.config))
                            allLocs
                    Just Unsat -> do
                        -- TODO what should we tell to the cache?
                        throwConclusion $ Right ()
                    _ -> do
                        return ()

-- TODO return units when they become available with more granularity
backendCoreOfflineHyp
    :: forall m i. (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => OfflineSolversConfig -> SMTProofCheckGroupWithFingerprints i -> (Units, ConclusionT (SMTProofCheckResult i ()) m ())
backendCoreOfflineHyp config group = (units, concM)
  where
    units = Units (numOfflineSolverConfigsForScope SolverScopeHyp config.groups)
    concM = do
        hypsConclusion <- lift . runConclusionT $ do
            for_ (ungroupSMTProofCheckGroup group.inner) $ \check -> do
                withPushLogContextCheck check $ do
                    hypConclusion <- lift . runConclusionT $ runSolver''' config check
                    case hypConclusion of
                        Nothing -> throwConclusion Nothing
                        Just (Right ()) -> return ()
                        Just (Left satLoc) -> throwConclusion (Just satLoc)
        case hypsConclusion of
            Nothing -> throwConclusion (Right ())
            Just (Just err) -> throwConclusion (Left err)
            Just Nothing -> return ()

backendCoreSingleCheck
    :: forall m i. (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => OfflineSolversConfig -> Throttle ->  SMTProofCheckWithFingerprint i -> m (SMTProofCheckResult i ())
backendCoreSingleCheck config throttle check =
    withThrottleUnliftIO throttle defaultPriority units $ do
        maybeConc <- runConclusionT $ runSolver''' config check
        return $ case maybeConc of
            Nothing -> Left $ SMTProofCheckError AllSolversTimedOutOrAnsweredUnknown (SMTProofCheckSourceCheck check.imp.meta)
            Just conc -> conc
  where
    units = Units (numOfflineSolverConfigsForScope SolverScopeHyp config.groups)

keepUncached
    :: (MonadLoggerWithContext m, MonadCache m, MonadError (SMTProofCheckError i) m)
    => SMTProofCheckGroupWithFingerprints i
    -> m (SMTProofCheckGroupWithFingerprints i)
keepUncached group = forOf (#inner % #imps) group $ \imps ->
    flip filterM imps (\imp -> do
        let fingerprint = imp.meta.fingerprint
        withPushLogContextCheckFingerprint fingerprint $ do
            cached <- queryCacheUsingFingerprint fingerprint
            case cached of
                Nothing -> return True
                Just AcceptableSatResultUnsat -> return False
                Just AcceptableSatResultSat -> throwError $ SMTProofCheckError
                    (SomeSolverAnsweredSat Cache)
                    (SMTProofCheckSourceCheck imp.meta))

withPushLogContextOfflineSolver :: MonadLoggerWithContext m => OfflineSolverConfig -> m a -> m a
withPushLogContextOfflineSolver solver = withPushLogContext ("solver " ++ solver.commandName ++ " " ++ memMode)
    where
    memMode = case solver.config.memoryMode of
        SolverMemoryModeWord8 -> "word8"
        SolverMemoryModeWord32 -> "word32"

runSolver' :: (MonadUnliftIO m, MonadMask m, MonadLoggerWithContext m, MonadCache m) => [String] -> SolverT m a -> m (a, Elapsed)
runSolver' cmd soverM = do
    logDebug "running solver"
    time $ runSolverWithLogging
        (uncurry proc (fromJust (uncons cmd)))
        soverM

runSolver'' :: (MonadUnliftIO m, MonadMask m, MonadLoggerWithContext m, MonadCache m) => [String] -> SolverT m (Maybe SatResult) -> m (Maybe SatResult)
runSolver'' cmd soverM = do
    (checkSatOutcome, elapsed) <- runSolver' cmd soverM
    let elapsedSuffix = makeElapsedSuffix elapsed
    case checkSatOutcome of
        Nothing -> do
            logDebug "timeout"
        Just Sat -> do
            logDebug $ "answered sat" ++ elapsedSuffix
        Just Unsat -> do
            logDebug $ "answered unsat" ++ elapsedSuffix
        Just (Unknown reason) -> do
            logDebug $ "answered unknown: " ++ showSExpr reason ++ " " ++ elapsedSuffix
    return checkSatOutcome

runSolver'''
    :: (MonadCache m, MonadUnliftIO m, MonadMask m, MonadLoggerWithContext m, MonadCache m)
    => OfflineSolversConfig -> SMTProofCheckWithFingerprint i -> ConclusionT (SMTProofCheckResult i ()) m ()
runSolver''' config check =
    runConcurrentlyUnliftIOC . for_ (offlineSolverConfigsForScope SolverScopeHyp config.groups) $ \solver ->
        makeConcurrentlyUnliftIOC . withPushLogContextOfflineSolver solver $ do
            checkSatOutcome <- lift . runSolver'' solver.command $ do
                executeSMTProofCheckOffline
                    (Just config.timeout)
                    solver.config
                    check
            case checkSatOutcome of
                Just Sat -> do
                    updateCache AcceptableSatResultSat check
                    throwConclusion . Left $ SMTProofCheckError
                        (SomeSolverAnsweredSat (OfflineSolver solver.commandName solver.config))
                        (SMTProofCheckSourceCheck check.imp.meta)
                Just Unsat -> do
                    updateCache AcceptableSatResultUnsat check
                    throwConclusion $ Right ()
                _ -> do
                    return ()

makeElapsedSuffix :: Elapsed -> String
makeElapsedSuffix elapsed = printf " (%.2fs)" (fromRational (elapsedToSeconds elapsed) :: Double)

--

type ConclusionT c m a = ExceptT c m a

runConclusionT :: Monad m => ConclusionT c m () -> m (Maybe c)
runConclusionT m = preview _Left <$> runExceptT m

throwConclusion :: Monad m => c -> ConclusionT c m ()
throwConclusion = throwError

type ConcurrentlyUnliftIOC m c = ConcurrentlyUnliftIOE m c ()

makeConcurrentlyUnliftIOC :: MonadUnliftIO m => ConclusionT c m () -> ConcurrentlyUnliftIOC m c
makeConcurrentlyUnliftIOC m = makeConcurrentlyUnliftIOE (runExceptT m)

runConcurrentlyUnliftIOC :: MonadUnliftIO m => ConcurrentlyUnliftIOC m c -> ConclusionT c m ()
runConcurrentlyUnliftIOC m = ExceptT $ runConcurrentlyUnliftIOE m

concurrentlyUnliftIOC_ :: MonadUnliftIO m => ConclusionT c m () -> ConclusionT c m () -> ConclusionT c m ()
concurrentlyUnliftIOC_ left right = runConcurrentlyUnliftIOC $ makeConcurrentlyUnliftIOC left *> makeConcurrentlyUnliftIOC right
