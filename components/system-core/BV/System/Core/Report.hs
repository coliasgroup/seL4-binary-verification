module BV.System.Core.Report
    ( CheckFailure (..)
    , CheckFailureCause (..)
    , CheckFailureCauseSolverId (..)
    , CheckFailureSource (..)
    , CheckResult
    , Report (..)
    , displayReport
    , prettyCheckFailure
    ) where

import BV.Core.Prelude
import BV.System.Core.Fingerprinting
import BV.System.Core.Types

import Data.Foldable (toList)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.Monoid.Extra (mwhen)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics
import Text.Printf (printf)

type CheckResult = Either CheckFailure ()

data CheckFailure
  = CheckFailure
      { cause :: CheckFailureCause
      , source :: CheckFailureSource
      }
  deriving (Eq, Generic, Ord, Show)

data CheckFailureCause
  = SomeSolverAnsweredSat CheckFailureCauseSolverId
  | AllSolversTimedOutOrAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

data CheckFailureCauseSolverId
  = OnlineSolver
  | OfflineSolver
      { offlineSolverCommandName :: String
      , modelConfig :: ModelConfig
      }
  | Cache
  deriving (Eq, Generic, Ord, Show)

data CheckFailureSource
  = CheckFailureSourceCheck Check
  | CheckFailureSourceCheckSubgroup CheckSubgroup
  deriving (Eq, Generic, Ord, Show)

prettyCheckFailureCauseSolverId :: CheckFailureCauseSolverId -> String
prettyCheckFailureCauseSolverId = \case
    OnlineSolver -> "online solver"
    OfflineSolver name modelConfig -> printf "offline solver (%s, %s)" name (prettyModelConfig modelConfig)
    Cache -> "cache"

prettyCheckFailure :: CheckFailure -> String
prettyCheckFailure err =
    prettyCause <> " for " <> prettySource
  where
    prettySource = case err.source of
        CheckFailureSourceCheck check ->
            "check " <> prettyCheckFingerprintShort check.fingerprint <> " @{" <> prettyProofCheckMeta check <> "}"
        CheckFailureSourceCheckSubgroup subgroup ->
            "check subgroup " <> prettyCheckSubgroupIdShort (takeSubgroupId subgroup) <> " (checks "
            <> mconcat (intersperse ","
                [ prettyCheckFingerprintShort check.fingerprint <> " @{" <> prettyProofCheckMeta check <> "}"
                | check <- toList subgroup.checks
                ])
            <> ")"
    prettyCause = case err.cause of
        SomeSolverAnsweredSat solverId -> prettyCheckFailureCauseSolverId solverId ++ " answered sat"
        AllSolversTimedOutOrAnsweredUnknown -> "all solvers timed out or answered unknown"

data Report
  = Report
      { unwrap :: M.Map PairingId' (Maybe CheckResult)
      }
  deriving (Eq, Generic, Ord, Show)

displayReport :: Report -> (Bool, String)
displayReport report =
    if M.null failed && S.null skipped
    then (True, "All checks passed\n")
    else
        let failedMsg =
                let perFailure = flip foldMap (M.toList failed) $ \(pairingId, err) ->
                        "Check failure for " <> prettyPairingId pairingId <> ": " <> prettyCheckFailure err <> "\n"
                 in perFailure <> "Some checks failed\n"
            skippedMsg = "Some checks skipped\n"
            msg = mwhen (not (M.null failed)) failedMsg <> mwhen (not (S.null skipped)) skippedMsg
         in (False, msg)
  where
    failed = M.mapMaybe (preview (_Just % _Left)) report.unwrap
    skipped = M.keysSet $ M.filter isNothing report.unwrap
