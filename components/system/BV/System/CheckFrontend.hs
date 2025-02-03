{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.System.CheckFrontend
    ( CheckReport (..)
    , SMTProofCheckError (..)
    , SMTProofCheckErrorWithDescriptions
    , SMTProofCheckResult
    , SMTProofCheckTask
    , checkFrontend
    ) where

import BV.Core.AdornProofScript
import BV.Core.Types
import BV.System.CheckFingerprint
import BV.System.TaskQueue
import BV.System.Utils
import BV.System.Utils.UnliftIO.Async

import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently),
                                 ConcurrentlyE (..))
import Control.Monad (void)
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO (withRunInIO))
import Control.Monad.Logger (MonadLogger, MonadLoggerIO, logInfoN)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics (Generic)
import Optics (ifor)
import Text.Printf (printf)

type SMTProofCheckTask = Task (SMTProofCheckGroup SMTProofCheckDescription) (SMTProofCheckResult ())

type SMTProofCheckResult a = Either SMTProofCheckErrorWithDescriptions a

type SMTProofCheckErrorWithDescriptions = (SMTProofCheckError, NonEmpty SMTProofCheckDescription)

data SMTProofCheckError
  = NoSolversAnswered
  | SomeSolverAnsweredSat
  | AllSolversAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

data CheckReport
  = CheckReport
      { unwrap :: M.Map PairingId (SMTProofCheckResult ())
      }
  deriving (Eq, Generic, Ord, Show)

checkFrontend
    :: ( MonadUnliftIO m
       , MonadLoggerIO m
       , SupportsTask (SMTProofCheckGroup SMTProofCheckDescription) (SMTProofCheckResult ()) t
       )
    => TaskQueueIn t
    -> FlattenedSMTProofChecks SMTProofCheckDescription
    -> m CheckReport
checkFrontend taskQueueIn checks = addLogContext' "frontend" $ do
    runConcurrentlyUnliftIO $ do
        CheckReport <$> ifor checks.unwrap (\pairingId checksForPairing -> concurrentlyUnliftIO $ do
            addLogContext' pairingId.asm.unwrap $ do
                runConcurrentlyUnliftIOE $ do
                    for_ checksForPairing (\group -> concurrentlyUnliftIOE $ do
                        addLogContext' (printf "group %.12v" (smtProofCheckGroupFingerprint group)) $ do
                            logTraceN "sending task"
                            result <- liftIO $ submitTaskAndWait taskQueueIn group
                            logInfoN . T.pack $ printf "task result: %s" (show result)
                            return result))
