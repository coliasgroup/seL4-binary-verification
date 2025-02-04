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
import BV.SMTLIB2.Process
import BV.SMTLIB2.SExpr.Build
import BV.System.Cache
import BV.System.Fingerprinting
import BV.System.Frontend
import BV.System.SolversConfig
import BV.System.Throttle
import BV.System.Utils.Logger
import BV.System.Utils.UnliftIO.Async
import BV.System.Utils.UnliftIO.Throttle

import BV.SMTLIB2.Command
import Control.Applicative (empty, (<|>))
import Control.Monad (filterM, forM_, unless)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT (ExceptT), MonadError, liftEither,
                             mapExceptT, runExceptT, throwError)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.State (evalState, state)
import Control.Monad.Trans (lift)
import Data.Foldable (sequenceA_)
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
offlineOnly = False
-- offlineOnly = True

backendCore
    :: forall m i. (MonadUnliftIO m, MonadLoggerAddContext m, MonadCache m, MonadMask m)
    => BackendCoreConfig -> Throttle -> SMTProofCheckGroup i -> m (SMTProofCheckResult i ())
backendCore config throttle group =
    runExceptT $ do
        filteredGroup <- filterGroupM group
        remaining <-
            if offlineOnly
            then return filteredGroup
            else addLoggerContext "online" $ do
                let filteredGroupWithLabels = zipWithT [0 :: Int ..] filteredGroup
                logInfo "running solver"
                onlineResults <-
                    lift $ withThrottleUnliftIO throttle (Priority 0) (Units 1) $ runSolver'
                        config.solversConfig.online.command
                        (executeSMTProofCheckGroupOnline
                            (Just (SolverTimeout config.solversConfig.onlineTimeout))
                            (SolverConfig { memoryMode = config.solversConfig.online.memoryMode })
                            filteredGroupWithLabels)
                let (exit, completed) = onlineResults
                forM_ completed $ \(i, _) -> do
                    let check = ungroupSMTProofCheckGroup filteredGroupWithLabels !! i
                    addLoggerContext (printf "check %.12v" (smtProofCheckFingerprint check)) $ do
                        logInfo "success"
                    updateCache check AcceptableSatResultUnsat
                case exit of
                    Right _ -> return ()
                    Left ((i, loc), abort) -> do
                        let check = ungroupSMTProofCheckGroup filteredGroupWithLabels !! i
                        addLoggerContext (printf "check %.12v" (smtProofCheckFingerprint check)) $ do
                            case abort of
                                OnlineSolverAbortReasonTimeout -> do
                                    logInfo "timeout"
                                    return ()
                                OnlineSolverAbortReasonAnsweredSat -> do
                                    logInfo "answered sat"
                                    updateCache (ungroupSMTProofCheckGroup filteredGroupWithLabels !! i) AcceptableSatResultSat
                                    throwError (SomeSolverAnsweredSat, loc :| [])
                                OnlineSolverAbortReasonAnsweredUnknown -> do
                                    logInfo "answered unknown"
                                    return ()
                let remaining = fmap snd
                        (filteredGroupWithLabels & #imps %~ filter ((\(i, _) -> i `notElem` map fst completed) . (.meta)))
                return remaining
        unless (null remaining.imps) $ addLoggerContext "offline" $ do
            let doAll = length remaining.imps > 1
            let units = numOfflineSolverConfigsForScope SolverScopeHyp config.solversConfig +
                    if doAll
                    then numOfflineSolverConfigsForScope SolverScopeAll config.solversConfig
                    else 0
            mapExceptT (withThrottleUnliftIO throttle (Priority 0) (Units units)) $ do
                let concAll =
                        if not doAll
                        then empty
                        else concurrentlyUnliftIO . runExceptT $ addLoggerContext "all" $ do
                            let locs = fromJust (nonEmpty (map (.meta) remaining.imps))
                            parallelResult <-
                                lift . runConcurrentlyUnliftIOE . sequenceA_ .
                                flip map (offlineSolverConfigsForScope SolverScopeAll config.solversConfig) $ \solver ->
                                    concurrentlyUnliftIOE $ addLoggerContext "solver _" $ do
                                        checkSatOutcome <- do
                                            logInfo "running solver"
                                            runSolver'
                                                solver.command
                                                (executeSMTProofCheckGroupOffline
                                                    (Just (SolverTimeout config.solversConfig.offlineTimeout))
                                                    (SolverConfig { memoryMode = solver.memoryMode })
                                                    remaining)
                                        case checkSatOutcome of
                                            Nothing -> do
                                                logInfo "timeout"
                                                return $ Right ()
                                            Just Sat -> do
                                                logInfo "answered sat"
                                                forM_ (ungroupSMTProofCheckGroup remaining) $ \check ->
                                                    updateCache check AcceptableSatResultSat
                                                return $ Left (Left (SomeSolverAnsweredSat, locs))
                                            Just Unsat -> do
                                                logInfo "answered unsat"
                                                -- updateCache check AcceptableSatResultUnsat
                                                return $ Left (Right ())
                                            Just Unknown -> do
                                                logInfo "answered unknown"
                                                return $ Right ()
                            case parallelResult of
                                Right () -> do
                                    throwError (AllSolversTimedOutOrAnsweredUnknown, locs)
                                Left definitiveAnswer -> do
                                    liftEither definitiveAnswer
                let concHyps = concurrentlyUnliftIO . runExceptT . addLoggerContext "hyp" $ do
                        forM_ (ungroupSMTProofCheckGroup remaining) $ \check -> do
                            let locs = check.imp.meta :| []
                            parallelResult <-
                                lift . runConcurrentlyUnliftIOE . sequenceA_ .
                                flip map (offlineSolverConfigsForScope SolverScopeHyp config.solversConfig) $ \solver ->
                                    concurrentlyUnliftIOE $ addLoggerContext "solver _" $ do
                                        checkSatOutcome <- do
                                            logInfo "running solver"
                                            runSolver'
                                                solver.command
                                                (executeSMTProofCheckOffline
                                                    (Just (SolverTimeout config.solversConfig.offlineTimeout))
                                                    (SolverConfig { memoryMode = solver.memoryMode })
                                                    check)
                                        case checkSatOutcome of
                                            Nothing -> do
                                                logInfo "timeout"
                                                return $ Right ()
                                            Just Sat -> do
                                                logInfo "answered sat"
                                                updateCache check AcceptableSatResultSat
                                                return $ Left (Left (SomeSolverAnsweredSat, locs))
                                            Just Unsat -> do
                                                logInfo "answered unsat"
                                                updateCache check AcceptableSatResultUnsat
                                                return $ Left (Right ())
                                            Just Unknown -> do
                                                logInfo "answered unknown"
                                                return $ Right ()
                            case parallelResult of
                                Right () -> do
                                    throwError (AllSolversTimedOutOrAnsweredUnknown, locs)
                                Left definitiveAnswer -> do
                                    liftEither definitiveAnswer
                ExceptT . runConcurrentlyUnliftIO $ concAll <|> concHyps
  where
    runSolver' cmd = runSolverWith
        modifyCtx
        stderrSink
        (uncurry proc (fromJust (uncons cmd)))
    stderrSink = logInfoGeneric . addLoggerContextToStr "stderr"
    modifyCtx ctx = SolverContext
        { sendSExpr = \req -> addLoggerContext "send" $ do
            logTraceGeneric . toLazyText $ buildSExpr req
            ctx.sendSExpr req
        , recvSExprWithTimeout = \timeout -> addLoggerContext "recv" $ do
            resp <- ctx.recvSExprWithTimeout timeout
            case resp of
                Nothing -> logTrace "timeout"
                Just sexpr -> logTraceGeneric . toLazyText $ buildSExpr sexpr
            return resp
        }

filterGroupM
    :: (MonadLogger m, MonadCache m, MonadError (SMTProofCheckError i) m)
    => SMTProofCheckGroup i
    -> m (SMTProofCheckGroup i)
filterGroupM group = forOf #imps group $ \imps ->
    -- TODO only reports first error
    flip filterM imps (\imp -> do
        let check = void $ SMTProofCheck group.setup imp
        cached <- queryCache check
        case cached of
            Nothing -> return True
            Just AcceptableSatResultUnsat -> return False
            Just AcceptableSatResultSat -> throwError (SomeSolverAnsweredSat, imp.meta :| []))

zipWithT :: Traversable f => [a] -> f b -> f (a, b)
zipWithT as f = flip evalState as $ traverse m f
  where
    m b = do
        a <- state (fromJust . uncons)
        return (a, b)
