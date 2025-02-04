{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}

module BV.System.Backend.Core
    ( BackendCoreConfig (..)
    , backendCore
    ) where

import BV.Core.ExecuteSMTProofChecks
import BV.Core.Types
import BV.SMTLIB2.Process
import BV.SMTLIB2
import BV.SMTLIB2.SExpr.Build
import BV.System.Cache
import BV.System.Fingerprinting
import BV.System.Frontend
import BV.System.SolversConfig
import BV.System.Throttle
import BV.System.Utils.Logger
import BV.System.Utils.UnliftIO.Throttle

import Control.Monad (filterM, forM_, unless)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.State (evalState, state)
import Control.Monad.Trans (lift)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty ((:|)))
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

backendCore
    :: (MonadUnliftIO m, MonadLoggerAddContext m, MonadCache m, MonadMask m)
    => BackendCoreConfig -> Throttle -> SMTProofCheckGroup i -> m (SMTProofCheckResult i ())
backendCore config throttle group =
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
        let (exit, completed) = onlineResults
        forM_ completed $ \(i, _) -> do
            let check = ungroupSMTProofCheckGroup filteredGroupWithLabels !! i
            addLoggerContext (printf "check %.12v" (smtProofCheckFingerprint check)) $ do
                logInfo "complete"
            updateCache check AcceptableSatResultUnsat
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
        unless (null remaining.imps) $ do
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
