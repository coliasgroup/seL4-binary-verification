{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# HLINT ignore "Redundant flip" #-}

module BV.System.Backend.Core
    ( BackendCoreConfig (..)
    , backendCore
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
import BV.System.Utils.StopWatch
import BV.System.Utils.UnliftIO.Async
import BV.System.Utils.UnliftIO.Throttle

import Control.Monad (filterM, unless)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT (ExceptT), MonadError, liftEither,
                             mapExceptT, runExceptT, throwError)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans (lift)
import Data.Foldable (for_)
import Data.Functor (void)
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
-- offlineOnly = False
offlineOnly = True

backendCore
    :: forall m i. (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => BackendCoreConfig -> Throttle -> SMTProofCheckGroup i -> m (SMTProofCheckResult i ())
backendCore config throttle group = runExceptT $ do
    uncached <- keepUncached group
    slow <-
        if offlineOnly
        then return uncached
        else withPushLogContext "online" $ do
            backendCoreOnline config.solversConfig throttle uncached
    unless (null slow.imps) $ withPushLogContext "offline" $ do
        backendCoreOffline config.solversConfig throttle slow

backendCoreOnline
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => SolversConfig -> Throttle -> SMTProofCheckGroup i -> ExceptT (SMTProofCheckError i) m (SMTProofCheckGroup i)
backendCoreOnline config throttle group = do
    let groupWithLabels = zipTraversableWith (,) [0 :: Int ..] group
    logInfo "running solver"
    onlineResults <-
        lift $ withThrottleUnliftIO throttle (Priority 0) (Units 1) $ runSolver'
            config.online.command
            (executeSMTProofCheckGroupOnline
                (Just (SolverTimeout config.onlineTimeout))
                (SolverConfig { memoryMode = config.online.memoryMode })
                groupWithLabels)
    let (exit, completed) = onlineResults
    for_ completed $ \(i, _) -> do
        let check = ungroupSMTProofCheckGroup groupWithLabels !! i
        withPushLogContext (printf "check %.12v" (smtProofCheckFingerprint check)) $ do
            logInfo "answered unsat"
        updateCache check AcceptableSatResultUnsat
    case exit of
        Right _ -> return ()
        Left ((i, loc), abort) -> do
            let check = ungroupSMTProofCheckGroup groupWithLabels !! i
            withPushLogContext (printf "check %.12v" (smtProofCheckFingerprint check)) $ do
                case abort of
                    OnlineSolverAbortReasonTimeout -> do
                        logInfo "timeout"
                        return ()
                    OnlineSolverAbortReasonAnsweredSat -> do
                        logInfo "answered sat"
                        updateCache (ungroupSMTProofCheckGroup groupWithLabels !! i) AcceptableSatResultSat
                        throwError (SomeSolverAnsweredSat, loc :| [])
                    OnlineSolverAbortReasonAnsweredUnknown -> do
                        logInfo "answered unknown"
                        return ()
    let remaining = fmap snd
            (groupWithLabels & #imps %~ filter ((\(i, _) -> i `notElem` map fst completed) . (.meta)))
    return remaining

-- TODO return units when they become available with more granularity
backendCoreOffline
    :: forall m i. (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => SolversConfig -> Throttle -> SMTProofCheckGroup i -> ExceptT (SMTProofCheckError i) m ()
backendCoreOffline config throttle group = do
    let doAll = length group.imps > 1
    let units = numOfflineSolverConfigsForScope SolverScopeHyp config +
            if doAll
            then numOfflineSolverConfigsForScope SolverScopeAll config
            else 0
    mapExceptT (withThrottleUnliftIO throttle (Priority 0) (Units units)) $ do
        let allLocs = fromJust (nonEmpty (map (.meta) group.imps))
        let concAll :: ConclusionT (SMTProofCheckResult i ()) m ()
            concAll =
                if not doAll
                then return ()
                else withPushLogContext "all" . runConcurrentlyUnliftIOC $ do
                    for_ (offlineSolverConfigsForScope SolverScopeAll config) $ \solver ->
                        makeConcurrentlyUnliftIOC . pushSolverLogContext solver $ do
                            logInfo "running solver"
                            (checkSatOutcome, elapsed) <- time . lift $ do
                                runSolver'
                                    solver.command
                                    (executeSMTProofCheckGroupOffline
                                        (Just (SolverTimeout config.offlineTimeout))
                                        (SolverConfig { memoryMode = solver.memoryMode })
                                        group)
                            let elapsedSuffix = makeElapsedSuffix elapsed
                            case checkSatOutcome of
                                Nothing -> do
                                    logInfo "timeout"
                                Just Sat -> do
                                    logInfo $ "answered sat" ++ elapsedSuffix
                                    for_ (ungroupSMTProofCheckGroup group) $ \check ->
                                        updateCache check AcceptableSatResultSat
                                    throwConclusion $ Left (SomeSolverAnsweredSat, allLocs)
                                Just Unsat -> do
                                    logInfo $ "answered unsat" ++ elapsedSuffix
                                    -- TODO what should we tell to the cache?
                                    throwConclusion $ Right ()
                                Just Unknown -> do
                                    logInfo $ "answered unknown" ++ elapsedSuffix
        let concHyps :: ConclusionT (SMTProofCheckResult i ()) m ()
            concHyps = do
                    hypsConclusion :: Maybe (Maybe i) <- lift . runConclusionT $ do
                        for_ (zip [1..] (ungroupSMTProofCheckGroup group)) $ \(hypLabel, check) ->
                            withPushLogContext ("hyp " ++ show hypLabel) $ do
                            -- let loc = check.imp.meta :| []
                                hypConclusion <- lift . runConclusionT $ do
                                    runConcurrentlyUnliftIOC .
                                        for_ (offlineSolverConfigsForScope SolverScopeHyp config) $ \solver ->
                                            makeConcurrentlyUnliftIOC . pushSolverLogContext solver $ do
                                                logInfo "running solver"
                                                (checkSatOutcome, elapsed) <- time . lift $ do
                                                    runSolver'
                                                        solver.command
                                                        (executeSMTProofCheckOffline
                                                            (Just (SolverTimeout config.offlineTimeout))
                                                            (SolverConfig { memoryMode = solver.memoryMode })
                                                            check)
                                                let elapsedSuffix = makeElapsedSuffix elapsed
                                                case checkSatOutcome of
                                                    Nothing -> do
                                                        logInfo "timeout"
                                                    Just Sat -> do
                                                        logInfo $ "answered sat" ++ elapsedSuffix
                                                        updateCache check AcceptableSatResultSat
                                                        throwConclusion $ Left check.imp.meta
                                                    Just Unsat -> do
                                                        logInfo $ "answered unsat" ++ elapsedSuffix
                                                        updateCache check AcceptableSatResultUnsat
                                                        throwConclusion $ Right ()
                                                    Just Unknown -> do
                                                        logInfo $ "answered unknown" ++ elapsedSuffix
                                case hypConclusion of
                                    Nothing -> throwConclusion Nothing
                                    Just (Right ()) -> return ()
                                    Just (Left satLoc) -> throwConclusion (Just satLoc)
                    case hypsConclusion of
                        Nothing -> throwConclusion (Right ())
                        Just (Just satLoc) -> throwConclusion (Left (SomeSolverAnsweredSat, satLoc :| []))
                        Just Nothing -> return ()
        conclusion <- lift . runConclusionT $ concurrentlyUnliftIOC_ concAll concHyps
        case conclusion of
            Nothing -> throwError (AllSolversTimedOutOrAnsweredUnknown, allLocs)
            Just c -> liftEither c
  where
    pushSolverLogContext solver = withPushLogContext ("solver " ++ solver.commandName ++ " " ++ memMode)
      where
        memMode = case solver.memoryMode of
            SolverMemoryModeWord8 -> "word8"
            SolverMemoryModeWord32 -> "word32"
    makeElapsedSuffix elapsed = printf " (%.2fs)" (fromRational (elapsedToSeconds elapsed) :: Double)

runSolver' :: (MonadUnliftIO m, MonadMask m, MonadLoggerWithContext m) => [String] -> SolverT m a -> m a
runSolver' cmd = runSolverWith
    modifyCtx
    stderrSink
    (uncurry proc (fromJust (uncons cmd)))
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

keepUncached
    :: (MonadLogger m, MonadCache m, MonadError (SMTProofCheckError i) m)
    => SMTProofCheckGroup i
    -> m (SMTProofCheckGroup i)
keepUncached group = forOf #imps group $ \imps ->
    -- TODO only reports first error
    flip filterM imps (\imp -> do
        let check = void $ SMTProofCheck group.setup imp
        cached <- queryCache check
        case cached of
            Nothing -> return True
            Just AcceptableSatResultUnsat -> return False
            Just AcceptableSatResultSat -> throwError (SomeSolverAnsweredSat, imp.meta :| []))

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
