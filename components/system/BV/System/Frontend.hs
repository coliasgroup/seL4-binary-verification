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
import BV.Core.Stages
import BV.Core.Types
import BV.System.Fingerprinting
import BV.System.Utils.Logger
import BV.System.Utils.UnliftIO.Async

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics (ifor)
import Text.Printf (printf)

type SMTProofCheckResult i a = Either (SMTProofCheckError i) a

type SMTProofCheckError i = (SMTProofCheckErrorCause, NonEmpty i)

data SMTProofCheckErrorCause
  = NoSolversAnswered
  | SomeSolverAnsweredSat
  | AllSolversAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

data Report
  = Report
      { unwrap :: M.Map PairingId (SMTProofCheckResult SMTProofCheckDescription ())
      }
  deriving (Eq, Generic, Ord, Show)

frontend
    :: ( MonadUnliftIO m
       , MonadLoggerAddContext m
       )
    => (SMTProofCheckGroup SMTProofCheckDescription -> m (SMTProofCheckResult SMTProofCheckDescription ()))
    -> PreparedSMTProofChecks
    -> m Report
frontend f checks =
    runConcurrentlyUnliftIO $ do
        Report <$> ifor checks.unwrap (\pairingId checksForPairing -> concurrentlyUnliftIO $ do
            addLoggerContext pairingId.asm.unwrap $ do
                runConcurrentlyUnliftIOE $ do
                    for_ checksForPairing (\group -> concurrentlyUnliftIOE $ do
                        addLoggerContext (printf "group %.12v" (smtProofCheckGroupFingerprint group)) $ do
                            logTrace "sending task"
                            result <- f group
                            logInfo $ printf "result: %s" (show result)
                            return result))
