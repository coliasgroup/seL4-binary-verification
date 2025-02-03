{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.System.CheckFrontend
    ( CheckReport (..)
    , SMTProofCheckError (..)
    , SMTProofCheckErrorWithDescriptions
    , SMTProofCheckResult
    , checkFrontend
    ) where

import BV.Core.AdornProofScript
import BV.Core.Stages
import BV.Core.Types
import BV.System.CheckFingerprint
import BV.System.TaskQueue
import BV.System.Utils.Logger
import BV.System.Utils.UnliftIO.Async

import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently),
                                 ConcurrentlyE (..))
import Control.Monad (void)
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO (withRunInIO))
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics (Generic)
import Optics (ifor)
import Text.Printf (printf)

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
       , MonadLoggerAddContext m
       )
    => (SMTProofCheckGroup SMTProofCheckDescription -> m (SMTProofCheckResult ()))
    -> PreparedSMTProofChecks
    -> m CheckReport
checkFrontend f checks =
    runConcurrentlyUnliftIO $ do
        CheckReport <$> ifor checks.unwrap (\pairingId checksForPairing -> concurrentlyUnliftIO $ do
            addLoggerContext pairingId.asm.unwrap $ do
                runConcurrentlyUnliftIOE $ do
                    for_ checksForPairing (\group -> concurrentlyUnliftIOE $ do
                        addLoggerContext (printf "group %.12v" (smtProofCheckGroupFingerprint group)) $ do
                            logTrace "sending task"
                            result <- f group
                            logInfo $ printf "result: %s" (show result)
                            return result))
