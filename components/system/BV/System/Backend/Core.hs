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
import BV.SMTLIB2
import BV.SMTLIB2.Command
import BV.SMTLIB2.Process
import BV.SMTLIB2.SExpr.Build
import BV.System.Cache
import BV.System.Fingerprinting
import BV.System.Frontend
import BV.System.SolversConfig
import BV.System.Throttle
import BV.System.Utils
import BV.System.Utils.Logger
import BV.System.Utils.Logger.BV
import BV.System.Utils.StopWatch
import BV.System.Utils.UnliftIO.Async
import BV.System.Utils.UnliftIO.Throttle
import BV.System.WithFingerprints

import Control.Monad (filterM, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT (ExceptT), MonadError, liftEither,
                             mapExceptT, runExceptT, throwError)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans (lift)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe (fromJust)
import Data.Text.Lazy.Builder
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
    slow <-
        if offlineOnly
        then return uncached
        else backendCoreOnline config.solversConfig.online throttle uncached
    backendCoreOffline config.solversConfig.offline throttle slow

backendCoreOnline
    :: forall m i. (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => OnlineSolverConfig -> Throttle -> SMTProofCheckGroupWithFingerprints i -> ExceptT (SMTProofCheckError i) m (SMTProofCheckGroupWithFingerprints i)
backendCoreOnline config throttle group = mapExceptT (withPushLogContext "online") $ do
    ((exit, completed :: [SMTProofCheckMetaWithFingerprint (Int, i)]), _elapsed) <-
        lift $ withThrottleUnliftIO throttle defaultPriority (Units 1) $ runSolver'
            config.command
            (executeSMTProofCheckGroupOnline
                (Just config.timeout)
                config.modelConfig
                groupWithLabels.inner)
    for_ (map (.inner) completed) $ \(i, _) -> do
        let check = checkAt i
        let fingerprint = check.imp.meta.fingerprint
        withPushLogContextCheck fingerprint $ do
            logInfo "answered unsat"
        updateCache fingerprint AcceptableSatResultUnsat
    case exit of
        Right () -> return ()
        Left (meta, abort) -> do
            let (i, _loc) = meta.inner
            let check = checkAt i
            let fingerprint = check.imp.meta.fingerprint
            withPushLogContextCheck fingerprint $ do
                case abort of
                    OnlineSolverAbortReasonTimeout -> do
                        logInfo "timeout"
                    OnlineSolverAbortReasonAnsweredSat -> do
                        logInfo "answered sat"
                        updateCache fingerprint AcceptableSatResultSat
                        throwError (SomeSolverAnsweredSat, fmap snd meta :| [])
                    OnlineSolverAbortReasonAnsweredUnknown reason -> do
                        logInfo $ "answered unknown: " ++ showSExpr reason
    let remaining = fmap snd
            (groupWithLabels & #inner % #imps %~ filter ((\(i, _) -> i `notElem` map (fst . (.inner)) completed) . (.inner) . (.meta)))
    return remaining
  where
    groupWithLabels = zipTraversableWithOf (#inner % #imps % traversed % #meta % #inner) (,) [0 :: Int ..] group
    checkAt i = (#inner %~ snd) <$> ungroupSMTProofCheckGroup groupWithLabels.inner !! i

-- TODO return units when they become available with more granularity
backendCoreOffline
    :: forall m i. (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => OfflineSolversConfig -> Throttle -> SMTProofCheckGroupWithFingerprints i -> ExceptT (SMTProofCheckError i) m ()
backendCoreOffline config throttle group =
    mapExceptT (withPushLogContext "offline") $ do
        when doAny . mapExceptT (withThrottleUnliftIO throttle defaultPriority unitsTotal) $ do
            conclusion <- lift . runConclusionT $ concurrentlyUnliftIOC_ concAll concHyp
            case conclusion of
                Nothing -> throwError (AllSolversTimedOutOrAnsweredUnknown, allLocs)
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
    allLocs = fromJust (nonEmpty (map (.meta) group.inner.imps))

-- TODO return units when they become available with more granularity
backendCoreOfflineAll
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => OfflineSolversConfig -> SMTProofCheckGroupWithFingerprints i -> (Units, ConclusionT (SMTProofCheckResult i ()) m ())
backendCoreOfflineAll config group = (units, concM)
  where
    units = Units (numOfflineSolverConfigsForScope SolverScopeAll config.groups)
    allLocs = fromJust (nonEmpty (map (.meta) group.inner.imps))
    concM = withPushLogContext "all" . runConcurrentlyUnliftIOC $ do
        for_ (offlineSolverConfigsForScope SolverScopeAll config.groups) $ \solver ->
            makeConcurrentlyUnliftIOC . pushSolverLogContext solver $ do
                (checkSatOutcome, elapsed) <- lift $ runSolver'
                    solver.command
                    (executeSMTProofCheckGroupOffline
                        (Just config.timeout)
                        solver.config
                        group.inner)
                let elapsedSuffix = makeElapsedSuffix elapsed
                case checkSatOutcome of
                    Nothing -> do
                        logInfo "timeout"
                    Just Sat -> do
                        logInfo $ "answered sat" ++ elapsedSuffix
                        for_ (ungroupSMTProofCheckGroup group.inner) $ \check -> do
                            let fingerprint = check.imp.meta.fingerprint
                            updateCache fingerprint AcceptableSatResultSat
                        throwConclusion $ Left (SomeSolverAnsweredSat, allLocs)
                    Just Unsat -> do
                        logInfo $ "answered unsat" ++ elapsedSuffix
                        -- TODO what should we tell to the cache?
                        throwConclusion $ Right ()
                    Just (Unknown reason) -> do
                        logInfo $ "answered unknown: " ++ showSExpr reason ++ " " ++ elapsedSuffix

-- TODO return units when they become available with more granularity
backendCoreOfflineHyp
    :: forall m i. (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => OfflineSolversConfig -> SMTProofCheckGroupWithFingerprints i -> (Units, ConclusionT (SMTProofCheckResult i ()) m ())
backendCoreOfflineHyp config group = (units, concM)
  where
    units = Units (numOfflineSolverConfigsForScope SolverScopeHyp config.groups)
    concM = do
        hypsConclusion :: Maybe (Maybe (SMTProofCheckMetaWithFingerprint i)) <- lift . runConclusionT $ do
            for_ (ungroupSMTProofCheckGroup group.inner) $ \check -> do
                let fingerprint = check.imp.meta.fingerprint
                withPushLogContextCheck fingerprint $ do
                    hypConclusion <- lift . runConclusionT $ do
                        runConcurrentlyUnliftIOC .
                            for_ (offlineSolverConfigsForScope SolverScopeHyp config.groups) $ \solver ->
                                makeConcurrentlyUnliftIOC . pushSolverLogContext solver $ do
                                    (checkSatOutcome, elapsed) <- lift $ runSolver'
                                        solver.command
                                        (executeSMTProofCheckOffline
                                            (Just config.timeout)
                                            solver.config
                                            check)
                                    let elapsedSuffix = makeElapsedSuffix elapsed
                                    case checkSatOutcome of
                                        Nothing -> do
                                            logInfo "timeout"
                                        Just Sat -> do
                                            logInfo $ "answered sat" ++ elapsedSuffix
                                            updateCache fingerprint AcceptableSatResultSat
                                            throwConclusion $ Left check.imp.meta
                                        Just Unsat -> do
                                            logInfo $ "answered unsat" ++ elapsedSuffix
                                            updateCache fingerprint AcceptableSatResultUnsat
                                            throwConclusion $ Right ()
                                        Just (Unknown reason) -> do
                                            logInfo $ "answered unknown: " ++ showSExpr reason ++ " " ++ elapsedSuffix
                    case hypConclusion of
                        Nothing -> throwConclusion Nothing
                        Just (Right ()) -> return ()
                        Just (Left satLoc) -> throwConclusion (Just satLoc)
        case hypsConclusion of
            Nothing -> throwConclusion (Right ())
            Just (Just satLoc) -> throwConclusion (Left (SomeSolverAnsweredSat, satLoc :| []))
            Just Nothing -> return ()

backendCoreSingleCheck
    :: forall m i. (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => SolversConfig -> SolverScope -> Throttle ->  SMTProofCheckWithFingerprint i -> m (SMTProofCheckResult i ())
backendCoreSingleCheck config solverScope throttle check =
    withThrottleUnliftIO throttle defaultPriority units $ do
        maybeConc <- runConclusionT $ do
            runConcurrentlyUnliftIOC . for_ (offlineSolverConfigsForScope SolverScopeHyp config.offline.groups) $ \solver ->
                makeConcurrentlyUnliftIOC . pushSolverLogContext solver $ do
                    (checkSatOutcome, elapsed) <- lift $ runSolver'
                        solver.command
                        (executeSMTProofCheckOffline
                            (Just config.offline.timeout)
                            solver.config
                            check)
                    let elapsedSuffix = makeElapsedSuffix elapsed
                    case checkSatOutcome of
                        Nothing -> do
                            logInfo "timeout"
                        Just Sat -> do
                            logInfo $ "answered sat" ++ elapsedSuffix
                            updateCache fingerprint AcceptableSatResultSat
                            throwConclusion $ Left (SomeSolverAnsweredSat, check.imp.meta :| [])
                        Just Unsat -> do
                            logInfo $ "answered unsat" ++ elapsedSuffix
                            updateCache fingerprint AcceptableSatResultUnsat
                            throwConclusion $ Right ()
                        Just (Unknown reason) -> do
                            logInfo $ "answered unknown: " ++ showSExpr reason ++ " " ++ elapsedSuffix
        return $ case maybeConc of
            Nothing -> Left (AllSolversTimedOutOrAnsweredUnknown, check.imp.meta :| [])
            Just conc -> conc
  where
    fingerprint = check.imp.meta.fingerprint
    units = Units (numOfflineSolverConfigsForScope solverScope config.offline.groups)

keepUncached
    :: (MonadLoggerWithContext m, MonadCache m, MonadError (SMTProofCheckError i) m)
    => SMTProofCheckGroupWithFingerprints i
    -> m (SMTProofCheckGroupWithFingerprints i)
keepUncached group = forOf (#inner % #imps) group $ \imps ->
    -- TODO only reports first error
    -- TODO log
    flip filterM imps (\imp -> do
        let fingerprint = imp.meta.fingerprint
        withPushLogContextCheck fingerprint $ do
            cached <- queryCache fingerprint
            case cached of
                Nothing -> return True
                Just AcceptableSatResultUnsat -> return False
                Just AcceptableSatResultSat -> throwError (SomeSolverAnsweredSat, imp.meta :| []))

pushSolverLogContext :: MonadLoggerWithContext m => OfflineSolverConfig -> m a -> m a
pushSolverLogContext solver = withPushLogContext ("solver " ++ solver.commandName ++ " " ++ memMode)
    where
    memMode = case solver.config.memoryMode of
        SolverMemoryModeWord8 -> "word8"
        SolverMemoryModeWord32 -> "word32"

runSolver' :: (MonadUnliftIO m, MonadMask m, MonadLoggerWithContext m) => [String] -> SolverT m a -> m (a, Elapsed)
runSolver' cmd soverM = time $ do
    logInfo "running solver"
    runSolverWith
        modifyCtx
        stderrSink
        (uncurry proc (fromJust (uncons cmd)))
        soverM
  where
    stderrSink = withPushLogContext "stderr" . logInfoGeneric
    modifyCtx ctx = SolverContext
        { sendSExpr = \req -> withPushLogContext "send" $ do
            logTraceGeneric . toLazyText $ buildSExpr req
            ctx.sendSExpr req
        , recvSExprWithTimeout = \timeout -> withPushLogContext "recv" $ do
            resp <- ctx.recvSExprWithTimeout timeout
            case resp of
                Nothing -> logTrace "timeout"
                Just sexpr -> logTraceGeneric . toLazyText $ buildSExpr sexpr
            return resp
        }

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
