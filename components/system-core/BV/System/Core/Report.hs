module BV.System.Core.Report
    ( Report (..)
    , SMTProofCheckFailure (..)
    , SMTProofCheckFailureCause (..)
    , SMTProofCheckFailureCauseSolverId (..)
    , SMTProofCheckFailureSource (..)
    , SMTProofCheckResult
    , displayReport
    , prettySMTProofCheckFailure
    ) where

import BV.Core.DecorateProofScript
import BV.Core.ExecuteSMTProofChecks
import BV.Core.Types
import BV.System.Core.Fingerprinting
import BV.System.Core.WithFingerprints

import Data.List (intersperse)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics
import Text.Printf (printf)

type SMTProofCheckResult i a = Either (SMTProofCheckFailure i) a

data SMTProofCheckFailure i
  = SMTProofCheckFailure
      { cause :: SMTProofCheckFailureCause
      , source :: SMTProofCheckFailureSource i
      }
  deriving (Eq, Generic, Ord, Show)

data SMTProofCheckFailureCause
  = SomeSolverAnsweredSat SMTProofCheckFailureCauseSolverId
  | AllSolversTimedOutOrAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

data SMTProofCheckFailureCauseSolverId
  = OnlineSolver
  | OfflineSolver
      { offlineSolverCommandName :: String
      , modelConfig :: ModelConfig
      }
  | Cache
  deriving (Eq, Generic, Ord, Show)

data SMTProofCheckFailureSource i
  = SMTProofCheckFailureSourceCheck (SMTProofCheckMetaWithFingerprint i)
  | SMTProofCheckFailureSourceCheckSubgroup SMTProofCheckGroupFingerprint [SMTProofCheckMetaWithFingerprint i]
  deriving (Eq, Generic, Ord, Show)

prettySolverId :: SMTProofCheckFailureCauseSolverId -> String
prettySolverId = \case
    OnlineSolver -> "online solver"
    OfflineSolver name modelConfig -> printf "offline solver (%s, %s)" name (prettyModelConfig modelConfig)
    Cache -> "cache"

prettySMTProofCheckFailure :: SMTProofCheckFailure SMTProofCheckDescription -> String
prettySMTProofCheckFailure err =
    prettyCause <> " for " <> prettySource
  where
    prettySource = case err.source of
        SMTProofCheckFailureSourceCheck check ->
            "check " <> prettySMTProofCheckFingerprintShort check.fingerprint
        SMTProofCheckFailureSourceCheckSubgroup group checks ->
            "some check in ["
            <> mconcat (intersperse ","
                [ prettySMTProofCheckFingerprintShort check.fingerprint
                | check <- checks
                ])
            <> "] (group " <> prettySMTProofCheckGroupFingerprintShort group <> ")"
    prettyCause = case err.cause of
        SomeSolverAnsweredSat solverId -> prettySolverId solverId ++ " answered sat"
        AllSolversTimedOutOrAnsweredUnknown -> "all solvers timed out or answered unknown"

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
                "Check failure for " <> prettyPairingId pairingId <> ": " <> prettySMTProofCheckFailure err <> "\n"
         in (True, failures <> "Some checks failed\n")
  where
    failed = M.mapMaybe (preview _Left) report.unwrap
