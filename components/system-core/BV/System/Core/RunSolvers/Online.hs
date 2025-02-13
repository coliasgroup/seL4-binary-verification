module BV.System.Core.RunSolvers.Online
    (
    ) where

import BV.Core.ExecuteSMTProofChecks
import BV.Core.Types
import BV.Logging
import BV.SMTLIB2
import BV.SMTLIB2.Command
import BV.SMTLIB2.Process
-- import BV.System.Cache
-- import BV.System.Frontend
-- import BV.System.SolversConfig
-- import BV.System.Throttle
-- import BV.System.Utils
-- import BV.System.Utils.Logger.BV
-- import BV.System.Utils.Stopwatch
-- import BV.System.Utils.UnliftIO.Async
-- import BV.System.Utils.UnliftIO.Throttle
-- import BV.System.WithFingerprints

import Control.Monad (filterM, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT (ExceptT), MonadError, liftEither,
                             mapExceptT, runExceptT, throwError)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans (lift)
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Optics
import System.Process (proc)
import Text.Printf (printf)

-- backendCoreOnline
--     :: forall m i. (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
--     => OnlineSolverConfig -> SMTProofCheckGroupWithFingerprints i -> ExceptT (SMTProofCheckError i) m (SMTProofCheckGroupWithFingerprints i)
-- backendCoreOnline config throttle group = mapExceptT (withPushLogContext "online") $ do
--     ((exit, completed :: [SMTProofCheckMetaWithFingerprint (Int, i)]), _elapsed) <-
--         lift $ withThrottleUnliftIO throttle defaultPriority (Units 1) $ runSolver'
--             config.command
--             (executeSMTProofCheckGroupOnline
--                 (Just config.timeout)
--                 config.modelConfig
--                 groupWithLabels.inner)
--     for_ (map (.inner) completed) $ \(i, _) -> do
--         let check = checkAt i
--         withPushLogContextCheck check $ do
--             logDebug "answered unsat"
--         updateCache AcceptableSatResultUnsat check
--     case exit of
--         Right () -> return ()
--         Left (meta, abort) -> do
--             let (i, _loc) = meta.inner
--             let check = checkAt i
--             withPushLogContextCheck check $ do
--                 case abort of
--                     OnlineSolverAbortReasonTimeout -> do
--                         logDebug "timeout"
--                     OnlineSolverAbortReasonAnsweredSat -> do
--                         logDebug "answered sat"
--                         updateCache AcceptableSatResultSat check
--                         throwError $ SMTProofCheckError
--                             (SomeSolverAnsweredSat OnlineSolver)
--                             (SMTProofCheckSourceCheck (fmap snd meta))
--                     OnlineSolverAbortReasonAnsweredUnknown reason -> do
--                         logDebug $ "answered unknown: " ++ showSExpr reason
--     let remaining = fmap snd
--             (groupWithLabels & #inner % #imps %~ filter ((\(i, _) -> i `notElem` map (fst . (.inner)) completed) . (.inner) . (.meta)))
--     return remaining
--   where
--     groupWithLabels = zipTraversableWithOf (#inner % #imps % traversed % #meta % #inner) (,) [0 :: Int ..] group
--     checkAt i = (#inner %~ snd) <$> ungroupSMTProofCheckGroup groupWithLabels.inner !! i
