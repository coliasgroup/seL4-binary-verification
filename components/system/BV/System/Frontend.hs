{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.System.Frontend
    ( Report (..)
    , SMTProofCheckError
    , SMTProofCheckErrorCause (..)
    , SMTProofCheckResult
    , frontend
    ) where

import BV.Core.AdornProofScript
import BV.Core.Types
import BV.System.Fingerprinting
import BV.System.Utils.Logger
import BV.System.Utils.Logger.BV
import BV.System.Utils.StopWatch
import BV.System.Utils.UnliftIO.Async
import BV.System.WithFingerprints

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics (ifor)
import Text.Printf (printf)

type SMTProofCheckResult i a = Either (SMTProofCheckError i) a

type SMTProofCheckError i = (SMTProofCheckErrorCause, NonEmpty (SMTProofCheckMetaWithFingerprint i))

data SMTProofCheckErrorCause
  = NoSolversAnswered
  | SomeSolverAnsweredSat
  | AllSolversTimedOutOrAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

data Report
  = Report
      { unwrap :: M.Map PairingId (SMTProofCheckResult SMTProofCheckDescription ())
      }
  deriving (Eq, Generic, Ord, Show)

frontend
    :: ( MonadUnliftIO m
       , MonadLoggerWithContext m
       )
    => (SMTProofCheckGroupWithFingerprints SMTProofCheckDescription -> m (SMTProofCheckResult SMTProofCheckDescription ()))
    -> FlattenedSMTProofChecksWithFingerprints SMTProofCheckDescription
    -> m Report
frontend f checks = do
    (report, elapsed) <- time . runConcurrentlyUnliftIO $ do
        Report <$> ifor checks.unwrap (\pairingId checksForPairing -> makeConcurrentlyUnliftIO $ do
            withPushLogContextPairing pairingId $ do
                runConcurrentlyUnliftIOE $ do
                    for_ checksForPairing (\group -> makeConcurrentlyUnliftIOE $ do
                        withPushLogContextCheckGroup group.fingerprint $ do
                            result <- f group
                            logInfo $ case result of
                                Right _ -> "success"
                                Left failure -> "failure: " ++ show failure
                            return result))
    logInfo $ printf "report complete after %.2fs" (fromRational (elapsedToSeconds elapsed) :: Double)
    return report
