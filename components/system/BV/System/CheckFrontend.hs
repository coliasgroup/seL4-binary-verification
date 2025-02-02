{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.System.CheckFrontend
    ( CheckReport (..)
    , SMTProofCheckError (..)
    , SMTProofCheckErrorWithLocations
    , SMTProofCheckResult
    , SMTProofCheckTask
    , checkFrontend
    ) where

import BV.Core.AdornProofScript
import BV.Core.Types
import BV.System.CheckFingerprint
import BV.System.TaskQueue

import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently),
                                 ConcurrentlyE (..))
import Control.Monad (void)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Control.Monad.Logger (MonadLogger, logInfoN)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics (Generic)
import Optics (ifor)
import Text.Printf (printf)

type SMTProofCheckTask = Task (SMTProofCheckGroup SMTProofCheckDescription) (SMTProofCheckResult ())

-- type SMTProofCheckResult = Maybe (NonEmpty SMTProofCheckErrorWithLocations)
type SMTProofCheckResult a = Either SMTProofCheckErrorWithLocations a

type SMTProofCheckErrorWithLocations = (SMTProofCheckError, NonEmpty SMTProofCheckDescription)

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
       , MonadLogger m
       , SupportsTask (SMTProofCheckGroup SMTProofCheckDescription) (SMTProofCheckResult ()) t
       )
    => TaskQueueIn t
    -> FlattenedSMTProofChecks SMTProofCheckDescription
    -> m CheckReport
checkFrontend taskQueueIn checks = withRunInIO $ \runInIO -> do
    runConcurrently $ do
        CheckReport <$> ifor checks.unwrap (\pairingId checksForPairing -> Concurrently $ do
            runConcurrentlyE $ do
                for_ checksForPairing (\group -> ConcurrentlyE $ do
                    result <- submitTaskAndWait taskQueueIn group
                    runInIO $ do
                        logInfoN . T.pack $ printf "Group result (%s): %s %s"
                            (prettyPairingId pairingId)
                            (smtProofCheckGroupFingerprint (void group))
                            (show result)
                    return result))
