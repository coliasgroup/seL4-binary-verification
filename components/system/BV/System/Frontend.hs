{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.System.Frontend
    ( Report (..)
    , SMTProofCheckError (..)
    , SMTProofCheckErrorCause (..)
    , SMTProofCheckErrorCauseSolverId (..)
    , SMTProofCheckResult
    , SMTProofCheckSource (..)
    , displayReport
    , frontend
    , frontendJustTheseChecks
    , prettySMTProofCheckError
    ) where

import BV.Core.DecorateProofScript
import BV.Core.Types
import BV.Logging
import BV.System.Core.Fingerprinting
import BV.System.Core.Report
import BV.System.Core.Utils.Logging
import BV.System.Core.WithFingerprints
import BV.System.SolversConfig
import BV.System.Utils.Stopwatch
import BV.System.Utils.UnliftIO.Async

import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Foldable (for_)
import Data.List (intersperse)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics
import Text.Printf (printf)

frontend
    :: ( MonadUnliftIO m
       , MonadLoggerWithContext m
       )
    => (SMTProofCheckGroupWithFingerprints SMTProofCheckDescription -> m (SMTProofCheckResult SMTProofCheckDescription ()))
    -> PreparedSMTProofChecksWithFingerprints
    -> m Report
frontend f checks = do
    let allGroups = checks ^.. #unwrap % folded % folded
    let numChecks = length allGroups
    completedGroups <- liftIO $ newTVarIO (0 :: Integer)
    (report, elapsed) <- time . runConcurrentlyUnliftIO $ do
        Report <$> ifor checks.unwrap (\pairingId checksForPairing -> makeConcurrentlyUnliftIO $ do
            withPushLogContextPairing pairingId $ do
                runConcurrentlyUnliftIOE $ do
                    for_ checksForPairing (\group -> makeConcurrentlyUnliftIOE $ do
                        withPushLogContextCheckGroup group $ do
                            result <- f group
                            logInfo $ case result of
                                Right _ -> "success"
                                Left failure -> "failure: " ++ prettySMTProofCheckError failure
                            n <- liftIO . atomically $ do
                                n' <- readTVar completedGroups
                                let n = n' + 1
                                writeTVar completedGroups n
                                return n
                            logInfo $ printf "%d/%d groups checked" n numChecks
                            return result))
    logInfo $ printf "report complete after %.2fs" (fromRational (elapsedToSeconds elapsed) :: Double)
    return report

frontendJustTheseChecks
    :: ( MonadUnliftIO m
       , MonadLoggerWithContext m
       )
    => (SMTProofCheckWithFingerprint SMTProofCheckDescription -> m (SMTProofCheckResult SMTProofCheckDescription ()))
    -> M.Map PairingId [SMTProofCheckWithFingerprint SMTProofCheckDescription]
    -> m Report
frontendJustTheseChecks f checks = do
    let allChecks = checks ^.. folded % folded
    let numChecks = length allChecks
    completedChecks <- liftIO $ newTVarIO (0 :: Integer)
    (report, elapsed) <- time . runConcurrentlyUnliftIO $ do
        Report <$> ifor checks (\pairingId checksForPairing -> makeConcurrentlyUnliftIO $ do
            withPushLogContextPairing pairingId $ do
                runConcurrentlyUnliftIOE $ do
                    for_ checksForPairing (\check -> makeConcurrentlyUnliftIOE $ do
                        withPushLogContextCheck check $ do
                            result <- f check
                            logInfo $ case result of
                                Right _ -> "success"
                                Left failure -> "failure: " ++ prettySMTProofCheckError failure
                            n <- liftIO . atomically $ do
                                n' <- readTVar completedChecks
                                let n = n' + 1
                                writeTVar completedChecks n
                                return n
                            logInfo $ printf "%d/%d checks checked" n numChecks
                            return result))
    logInfo $ printf "report complete after %.2fs" (fromRational (elapsedToSeconds elapsed) :: Double)
    return report
