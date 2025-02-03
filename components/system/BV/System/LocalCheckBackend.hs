{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}

module BV.System.LocalCheckBackend
    ( AcceptableSatResult (..)
    , LocalCheckBackendConfig (..)
    , MonadLocalCheckCache (..)
    , localCheckBackend
    ) where

import BV.Core.AdornProofScript
import BV.Core.ExecuteSMTProofChecks
import BV.Core.ExecuteSMTProofChecks (SolverConfig)
import BV.Core.Types
import BV.SMTLIB2.Process
import BV.SMTLIB2.Types
import BV.System.CheckFingerprint
import BV.System.CheckFrontend
import BV.System.LocalCheckBackend.Cache
import BV.System.SolversConfig
import BV.System.TaskQueue
import BV.System.Throttle
import BV.System.Utils.Logger
import BV.System.Utils.UnliftIO.Async
import BV.System.Utils.UnliftIO.Throttle

import BV.SMTLIB2.Builder (buildSExpr)
import BV.SMTLIB2.Process (SolverContext (SolverContext, recvWithTimeout),
                           runSolverWith)
import BV.System.Utils.Logger (logTraceGeneric)
import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently),
                                 race)
import Control.Monad (filterM, forM_, forever)
import Control.Monad.Catch (MonadMask, MonadThrow, SomeException)
import Control.Monad.Except (ExceptT (ExceptT), MonadError, runExceptT,
                             throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.State (evalState, state)
import Control.Monad.Trans (lift)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes, fromJust)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Lazy.Builder
import Data.Void (absurd)
import GHC.Generics (Generic)
import Optics
import System.Process (CreateProcess, proc)
import Text.Printf (printf)

data LocalCheckBackendConfig
  = LocalCheckBackendConfig
      { numCores :: Integer
      , solversConfig :: SolversConfig
      }
  deriving (Eq, Generic, Ord, Show)

localCheckBackend
    :: (MonadUnliftIO m, MonadLoggerAddContext m, MonadLocalCheckCache m, MonadMask m)
    => LocalCheckBackendConfig -> FlattenedSMTProofChecks SMTProofCheckDescription -> m CheckReport
localCheckBackend config checks = do
    (taskQueueIn, taskQueueControl) <- liftIO newTaskQueue
    let completeTasks = addLoggerContext "backend" $ do
            withThrottlingUnliftIO (Units config.numCores) $ \throttle -> do
                forever $ do
                    task <- liftIO $ acceptTask taskQueueControl
                    let (group, withResult) = useTask task
                    result <- checkGroup config throttle group
                    liftIO $ withResult result
    either absurd id <$> raceUnliftIO completeTasks (checkFrontend taskQueueIn checks)

checkGroup
    :: (MonadUnliftIO m, MonadLoggerAddContext m, MonadLocalCheckCache m, MonadMask m)
    => LocalCheckBackendConfig -> Throttle -> SMTProofCheckGroup SMTProofCheckDescription -> m (SMTProofCheckResult ())
checkGroup config throttle group =
    addLoggerContext (printf "group %.12v" (smtProofCheckGroupFingerprint group)) $ do
        runExceptT $ do
            logDebug "checking"
            filteredGroup <- filterGroupM group
            let filteredGroupWithLabels = zipWithT [0 :: Int ..] filteredGroup
            onlineResults <-
                lift $ withThrottleUnliftIO throttle (Priority 0) (Units 1) $ addLoggerContext "online solver" $ runSolverWith
                    modifyCtx
                    (uncurry proc (fromJust (uncons config.solversConfig.online.command)))
                    logStderr
                    (executeSMTProofCheckGroupOnline
                        (SolverConfig { memoryMode = config.solversConfig.online.memoryMode })
                        (Just (SolverTimeout config.solversConfig.onlineTimeout))
                        filteredGroupWithLabels)
            logInfo $ "results: " ++ show onlineResults
            let (exit, completed) = onlineResults
            -- let completedChecks = ungroupSMTProofCheckGroup filteredGroupWithLabels
            forM_ completed $ \(i, _) -> do
                updateCache (ungroupSMTProofCheckGroup filteredGroupWithLabels !! i) AcceptableSatResultUnsat
            case exit of
                Right _ -> return ()
                Left ((i, loc), abort) -> do
                    case abort of
                        OnlineSolverAbortReasonTimeout -> do
                            return ()
                        OnlineSolverAbortReasonAnsweredSat -> do
                            updateCache (ungroupSMTProofCheckGroup filteredGroupWithLabels !! i) AcceptableSatResultSat
                            throwError (SomeSolverAnsweredSat, loc :| [])
                        OnlineSolverAbortReasonAnsweredUnknown -> do
                            return ()
                    let remaining = filteredGroupWithLabels & #imps %~ filter ((\(i, _) -> i `notElem` map fst completed) . (.meta))
                    undefined
  where
    logStderr = logInfoGeneric . addLoggerContextToStr "stderr"
    modifyCtx ctx = SolverContext
        { send = \req -> addLoggerContext "send" $ do
            logTraceGeneric . toLazyText $ buildSExpr req
            ctx.send req
        , recvWithTimeout = \timeout -> addLoggerContext "recv" $ do
            resp <- ctx.recvWithTimeout timeout
            case resp of
                Nothing -> logTrace "timeout"
                Just sexpr -> logTraceGeneric . toLazyText $ buildSExpr sexpr
            return resp
        }

filterGroupM
    :: (MonadLogger m, MonadLocalCheckCache m, MonadError SMTProofCheckErrorWithDescriptions m)
    => SMTProofCheckGroup SMTProofCheckDescription
    -> m (SMTProofCheckGroup SMTProofCheckDescription)
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

-- online
--     :: ( MonadUnliftIO m
--        , MonadLogger m
--        , MonadThrow m
--        , MonadMask m
--        )
--     => SolverConfig
--     -> SMTProofCheckGroup a
--     -> m (Either SomeException (), [a])
-- online config group = do
--     undefined
