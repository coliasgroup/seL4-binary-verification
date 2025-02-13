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
import BV.System.Fingerprinting
import BV.System.SolversConfig
import BV.System.Utils.Logger.BV
import BV.System.Utils.Stopwatch
import BV.System.Utils.UnliftIO.Async
import BV.System.WithFingerprints

import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Foldable (for_)
import Data.List (intersperse)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics
import Text.Printf (printf)

type SMTProofCheckResult i a = Either (SMTProofCheckError i) a

data SMTProofCheckError i
  = SMTProofCheckError
      { cause :: SMTProofCheckErrorCause
      , source :: SMTProofCheckSource i
      }
  deriving (Eq, Generic, Ord, Show)

data SMTProofCheckErrorCause
  = SomeSolverAnsweredSat SMTProofCheckErrorCauseSolverId
  | AllSolversTimedOutOrAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

data SMTProofCheckErrorCauseSolverId
  = OnlineSolver
  | OfflineSolver OfflineSolverName SolverMemoryMode
  | Cache
  deriving (Eq, Generic, Ord, Show)

data SMTProofCheckSource i
  = SMTProofCheckSourceCheck (SMTProofCheckMetaWithFingerprint i)
  | SMTProofCheckSourceSyntheticGroup [SMTProofCheckMetaWithFingerprint i]
  deriving (Eq, Generic, Ord, Show)

prettySolverId :: SMTProofCheckErrorCauseSolverId -> String
prettySolverId = \case
    OnlineSolver -> "online solver"
    OfflineSolver name memMode -> printf "offline solver (%s, %s)" name (prettySolverMemoryMode memMode)
    Cache -> "cache"

prettySMTProofCheckError :: SMTProofCheckError SMTProofCheckDescription -> String
prettySMTProofCheckError err =
    prettyCause <> " for " <> prettySource
  where
    prettySource = case err.source of
        SMTProofCheckSourceCheck check ->
            "check " <> prettySMTProofCheckFingerprintShort check.fingerprint
        SMTProofCheckSourceSyntheticGroup checks ->
            "some check in ["
            <> mconcat (intersperse ","
                [ prettySMTProofCheckFingerprintShort check.fingerprint
                | check <- checks
                ])
            <> "]"
    prettyCause = case err.cause of
        SomeSolverAnsweredSat solverId -> prettySolverId solverId ++ " answered sat"
        AllSolversTimedOutOrAnsweredUnknown -> "all solvers timed out or answered unknown"

--

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

data Report
  = Report
      { unwrap :: M.Map PairingId (SMTProofCheckResult SMTProofCheckDescription ())
      }
  deriving (Eq, Generic, Ord, Show)

displayReport :: Report -> (Bool, String)
displayReport report =
    if M.null failed
    then (False, "All checks passed\n")
    else
        let failures = flip foldMap (M.toAscList failed) $ \(pairingId, err) ->
                "Check failure for " <> prettyPairingId pairingId <> ": " <> prettySMTProofCheckError err <> "\n"
         in (True, failures <> "Some checks failed\n")
  where
    failed = M.mapMaybe (preview _Left) report.unwrap

--

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
