{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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

import Control.Applicative (empty, (<|>))
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
    :: forall m i. (MonadUnliftIO m, MonadLoggerContextStack m, MonadCache m, MonadMask m)
    => BackendCoreConfig -> Throttle -> SMTProofCheckGroup i -> m (SMTProofCheckResult i ())
backendCore config throttle group = runExceptT $ do
    uncached <- keepUncached group
    slow <-
        if offlineOnly
        then return uncached
        else pushLogContext "online" $ do
            backendCoreOnline config.solversConfig throttle uncached
    unless (null slow.imps) $ pushLogContext "offline" $ do
        backendCoreOffline config.solversConfig throttle slow

backendCoreOnline
    :: (MonadUnliftIO m, MonadLoggerContextStack m, MonadCache m, MonadMask m)
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
        pushLogContext (printf "check %.12v" (smtProofCheckFingerprint check)) $ do
            logInfo "answered unsat"
        updateCache check AcceptableSatResultUnsat
    case exit of
        Right _ -> return ()
        Left ((i, loc), abort) -> do
            let check = ungroupSMTProofCheckGroup groupWithLabels !! i
            pushLogContext (printf "check %.12v" (smtProofCheckFingerprint check)) $ do
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

backendCoreOffline
    :: (MonadUnliftIO m, MonadLoggerContextStack m, MonadCache m, MonadMask m)
    => SolversConfig -> Throttle -> SMTProofCheckGroup i -> ExceptT (SMTProofCheckError i) m ()
backendCoreOffline config throttle group = do
    let doAll = length group.imps > 1
    let units = numOfflineSolverConfigsForScope SolverScopeHyp config +
            if doAll
            then numOfflineSolverConfigsForScope SolverScopeAll config
            else 0
    mapExceptT (withThrottleUnliftIO throttle (Priority 0) (Units units)) $ do
        let concAll =
                if not doAll
                then empty
                else concurrentlyUnliftIO . runExceptT . pushLogContext "all" $ do
                    let locs = fromJust (nonEmpty (map (.meta) group.imps))
                    parallelResult <-
                        lift . runConcurrentlyUnliftIOE .
                            for_ (offlineSolverConfigsForScope SolverScopeAll config) $ \solver ->
                                concurrentlyUnliftIOE . runExceptT . pushLogContext ("solver " ++ solver.commandName) $ do
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
                                            throwError $ Left (SomeSolverAnsweredSat, locs)
                                        Just Unsat -> do
                                            logInfo $ "answered unsat" ++ elapsedSuffix
                                            -- TODO what should we tell to the cache?
                                            throwError $ Right ()
                                        Just Unknown -> do
                                            logInfo $ "answered unknown" ++ elapsedSuffix
                    interpretParallelResult locs parallelResult
        let concHyps = concurrentlyUnliftIO . runExceptT . pushLogContext "hyp" $ do
                for_ (ungroupSMTProofCheckGroup group) $ \check -> do
                    let locs = check.imp.meta :| []
                    parallelResult <-
                        lift . runConcurrentlyUnliftIOE .
                            for_ (offlineSolverConfigsForScope SolverScopeHyp config) $ \solver ->
                                concurrentlyUnliftIOE . runExceptT . pushLogContext ("solver " ++ solver.commandName) $ do
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
                                            throwError $ Left (SomeSolverAnsweredSat, locs)
                                        Just Unsat -> do
                                            logInfo $ "answered unsat" ++ elapsedSuffix
                                            updateCache check AcceptableSatResultUnsat
                                            throwError $ Right ()
                                        Just Unknown -> do
                                            logInfo $ "answered unknown" ++ elapsedSuffix
                    interpretParallelResult locs parallelResult
        ExceptT . runConcurrentlyUnliftIO $ concAll <|> concHyps
  where
    makeElapsedSuffix elapsed = printf " (%.2fs)" (fromRational (elapsedToSeconds elapsed) :: Double)
    interpretParallelResult locs = \case
        Right () -> throwError (AllSolversTimedOutOrAnsweredUnknown, locs)
        Left definitiveAnswer -> liftEither definitiveAnswer

runSolver' :: (MonadUnliftIO m, MonadMask m, MonadLoggerContextStack m) => [String] -> SolverT m a -> m a
runSolver' cmd = runSolverWith
    modifyCtx
    stderrSink
    (uncurry proc (fromJust (uncons cmd)))
  where
    stderrSink = logInfoGeneric . addLoggerContextToStr "stderr"
    modifyCtx ctx = SolverContext
        { sendSExpr = \req -> pushLogContext "send" $ do
            logTraceGeneric . toLazyText $ buildSExpr req
            ctx.sendSExpr req
        , recvSExprWithTimeout = \timeout -> pushLogContext "recv" $ do
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
