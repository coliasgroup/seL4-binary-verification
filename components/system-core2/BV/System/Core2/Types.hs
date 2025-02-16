{-# LANGUAGE DeriveAnyClass #-}

module BV.System.Core2.Types
    ( Check (..)
    , CheckFilter (..)
    , CheckGroup (..)
    , CheckSubgroup (..)
    , CheckSubgroupId (..)
    , Checks (..)
    , elaborateChecks
    , filterChecks
    , prettyCheckSubgroupIdShort
    , takeSubgroupByFingerprint
    , takeSubgroupByIndex
    , takeSubgroupId
    ) where

import BV.Core
import BV.System.Core2.Fingerprinting

import Control.DeepSeq (NFData)
import Data.List (intercalate)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics

data Check
  = Check
      { fingerprint :: CheckFingerprint
      , group :: CheckGroup
      , proofCheckDescription :: ProofCheckDescription
      , imp :: SExprWithPlaceholders
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data CheckGroup
  = CheckGroup
      { fingerprint :: CheckGroupFingerprint
      , pairingId :: PairingId
      , proofScriptNodePath :: ProofScriptNodePath
      , setup :: [SExprWithPlaceholders]
      , checks :: [Check]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data CheckSubgroup
  = CheckSubgroup
      { group :: CheckGroup
      , checks :: [(Int, Check)]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data CheckSubgroupId
  = CheckSubgroupId
      { groupFingerprint :: CheckGroupFingerprint
      , checkIndices :: [Int]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

newtype Checks
  = Checks { unwrap :: M.Map PairingId [CheckSubgroup] }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

elaborateChecks :: StagesOutputChecks -> Checks
elaborateChecks stagesOutputChecks = Checks $ M.mapWithKey (map . f) stagesOutputChecks.unwrap
  where
    f pairingId (proofScriptNodePath, stagesOutputGroup) =
        let group = CheckGroup
                { fingerprint = fingerprintCheckGroup stagesOutputGroup
                , pairingId
                , proofScriptNodePath
                , setup = group.setup
                , checks =
                    [ Check
                        { fingerprint = fingerprintCheck (SMTProofCheck group.setup imp)
                        , group
                        , proofCheckDescription = imp.meta
                        , imp = imp.term
                        }
                    | imp <- stagesOutputGroup.imps
                    ]
                }
         in fullSubgroup group

fullSubgroup :: CheckGroup -> CheckSubgroup
fullSubgroup group = CheckSubgroup
    { group
    , checks = zip [0..] group.checks
    }

data CheckFilter
  = CheckFilter
      { pairings :: PairingId -> Bool
      , groups :: CheckGroupFingerprint -> Bool
      , checks :: CheckFingerprint -> Bool
      }
  deriving (Generic)

filterChecks :: CheckFilter -> Checks -> Checks
filterChecks checkFilter =
    #unwrap %~
        ((traversed %~ (
            (traversed %~ takeSubgroupByFingerprint checkFilter.checks)
                . filter (\subgroup -> checkFilter.groups subgroup.group.fingerprint)))
            . M.filterWithKey (\k _v -> checkFilter.pairings k))

takeSubgroupByIndex :: (Int -> Bool) -> CheckSubgroup -> CheckSubgroup
takeSubgroupByIndex p = #checks %~ filter (\(i, _check) -> p i)

takeSubgroupByFingerprint :: (CheckFingerprint -> Bool) -> CheckSubgroup -> CheckSubgroup
takeSubgroupByFingerprint p = #checks %~ filter (\(_i, check) -> p check.fingerprint)

takeSubgroupId :: CheckSubgroup -> CheckSubgroupId
takeSubgroupId subgroup =
    CheckSubgroupId
        { groupFingerprint = subgroup.group.fingerprint
        , checkIndices = map fst subgroup.checks
        }

prettyCheckSubgroupIdShort :: CheckSubgroupId -> String
prettyCheckSubgroupIdShort subgroupId =
    prettyCheckGroupFingerprintShort subgroupId.groupFingerprint
    ++ "("
    ++ intercalate "," (map show subgroupId.checkIndices)
    ++ ")"
