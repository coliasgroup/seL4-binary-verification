module BV.System.Core.Frontend
    ( frontend
    ) where

import BV.Logging
import BV.System.Core.Cache
import BV.System.Core.Report
import BV.System.Core.Solvers
import BV.System.Core.Types
import BV.System.Core.Utils.Logging
import BV.System.Utils.Stopwatch
import BV.System.Utils.UnliftIO.Async
import BV.Utils (fromIntegerChecked)

import Control.Concurrent (newQSem, signalQSem, waitQSem)
import Control.Concurrent.STM (newTVarIO, readTVar)
import Control.Concurrent.STM.TVar (writeTVar)
import Control.DeepSeq (deepseq)
import Control.Exception.Safe (bracket_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.STM (atomically)
import Data.Foldable (for_)
import Optics
import Text.Printf (printf)

frontend
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => Integer
    -> SolverGate m
    -> SolverBackend m
    -> SolversConfig
    -> Checks
    -> m Report
frontend numEvalCores gate backend config checks = do
    let numGroups = length (toListOf (#unwrap % folded % folded % folded) checks :: [CheckSubgroup])
    logInfo $ printf "%d groups to check" numGroups
    sem <- liftIO $ newQSem $ fromIntegerChecked numEvalCores
    completedGroups <- liftIO $ newTVarIO (0 :: Integer)
    (report, elapsed) <- time . runConcurrentlyUnliftIO $ do
        Report <$> ifor checks.unwrap (\pairingId checksForPairing -> makeConcurrentlyUnliftIO $ do
            withPushLogContextPairing pairingId $ do
                runConcurrentlyUnliftIOE $ do
                    for_ (checksForPairing ^.. folded % folded) (\subgroup -> makeConcurrentlyUnliftIOE $ do
                        liftIO $ bracket_ (waitQSem sem) (signalQSem sem) $ do
                            subgroup.group.fingerprint `deepseq` return ()
                        withPushLogContextCheckGroup subgroup.group $ do
                            result <- runSolvers gate backend config subgroup
                            logInfo $ case result of
                                Right _ -> "success"
                                Left failure -> "failure: " ++ prettyCheckFailure failure
                            n <- liftIO . atomically $ do
                                n' <- readTVar completedGroups
                                let n = n' + 1
                                writeTVar completedGroups n
                                return n
                            logInfo $ printf "%d/%d groups checked" n numGroups
                            return result))
    logInfo $ printf "report complete after %.2fs" (fromRational (elapsedToSeconds elapsed) :: Double)
    return report
