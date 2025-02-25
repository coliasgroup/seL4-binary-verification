{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.System.Core.Types
    ( Check (..)
    , CheckFilter (..)
    , CheckGroup (..)
    , CheckIndexInGroup (..)
    , CheckPath (..)
    , CheckSubgroup (..)
    , CheckSubgroupId (..)
    , CheckSubgroupPath (..)
    , Checks (..)
    , elaborateChecks
    , elaborateChecksFromInput
    , filterChecks
    , findCheck
    , findCheckSubgroup
    , pathForCheck
    , pathForCheckSubgroup
    , prettyCheckSubgroupIdShort
    , splitSubgroupAt
    , takeEmptySubgroup
    , takeSubgroupByFingerprint
    , takeSubgroupByIndexInGroup
    , takeSubgroupId
    , toCoreCheck
    , toCoreCheckGroup
    ) where

import BV.Core
import BV.System.Core.Fingerprinting
import BV.System.Core.Utils

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.List (genericSplitAt, intercalate)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics

data Check
  = Check
      { fingerprint :: CheckFingerprint
      , group :: CheckGroup
      , meta :: ProofCheckMeta
      , imp :: SExprWithPlaceholders
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data CheckGroup
  = CheckGroup
      { fingerprint :: CheckGroupFingerprint
      , pairingId :: PairingId
      , setup :: [SExprWithPlaceholders]
      , checks :: [Check]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

newtype CheckIndexInGroup
  = CheckIndexInGroup { unwrap :: Integer }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (Binary, NFData)

data CheckSubgroup
  = CheckSubgroup
      { group :: CheckGroup
      , checks :: [(CheckIndexInGroup, Check)]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data CheckSubgroupId
  = CheckSubgroupId
      { groupFingerprint :: CheckGroupFingerprint
      , checkIndices :: [CheckIndexInGroup]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary CheckSubgroupId where

newtype Checks
  = Checks { unwrap :: M.Map PairingId (M.Map CheckGroupFingerprint CheckSubgroup) }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

elaborateChecksFromInput :: StagesInput -> Checks
elaborateChecksFromInput input = elaborateChecks (stages input).checks

elaborateChecks :: StagesOutputChecks -> Checks
elaborateChecks stagesOutputChecks = Checks $ M.mapWithKey f stagesOutputChecks.unwrap
  where
    f pairingId stagesOutputGroups = M.fromList
        [ let group = CheckGroup
                { fingerprint = fingerprintCheckGroup stagesOutputGroup
                , pairingId
                , setup = stagesOutputGroup.setup
                , checks =
                    [ Check
                        { fingerprint = fingerprintCheck (SMTProofCheck stagesOutputGroup.setup imp)
                        , group
                        , meta = imp.meta
                        , imp = imp.term
                        }
                    | imp <- stagesOutputGroup.imps
                    ]
                }
           in (group.fingerprint, fullSubgroup group)
        | stagesOutputGroup <- stagesOutputGroups
        ]

fullSubgroup :: CheckGroup -> CheckSubgroup
fullSubgroup group = CheckSubgroup
    { group
    , checks = zip (map CheckIndexInGroup [0..]) group.checks
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
                . M.filterWithKey (\k _v -> checkFilter.groups k)))
            . M.filterWithKey (\k _v -> checkFilter.pairings k))

takeEmptySubgroup :: CheckSubgroup -> CheckSubgroup
takeEmptySubgroup = #checks .~ []

takeSubgroupByIndexInGroup :: (CheckIndexInGroup -> Bool) -> CheckSubgroup -> CheckSubgroup
takeSubgroupByIndexInGroup p = #checks %~ filter (\(i, _check) -> p i)

takeSubgroupByFingerprint :: (CheckFingerprint -> Bool) -> CheckSubgroup -> CheckSubgroup
takeSubgroupByFingerprint p = #checks %~ filter (\(_i, check) -> p check.fingerprint)

splitSubgroupAt :: Integer -> CheckSubgroup -> (CheckSubgroup, CheckSubgroup)
splitSubgroupAt i subgroup = (subgroup & #checks .~ l, subgroup & #checks .~ r)
  where
    (l, r) = genericSplitAt i subgroup.checks

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
    ++ intercalate "," (map (show . (.unwrap)) subgroupId.checkIndices)
    ++ ")"

--

toCoreCheck :: Check -> SMTProofCheck ()
toCoreCheck check = SMTProofCheck
    { setup = check.group.setup
    , imp = SMTProofCheckImp
        { meta = ()
        , term = check.imp
        }
    }

toCoreCheckGroup :: CheckSubgroup -> SMTProofCheckGroup ()
toCoreCheckGroup subgroup = SMTProofCheckGroup
    { setup = subgroup.group.setup
    , imps =
        [ SMTProofCheckImp
            { meta = ()
            , term = check.imp
            }
        | (_i, check) <- subgroup.checks
        ]
    }

--

data CheckPath
  = CheckPath
      { pairingId :: PairingId
      , groupFingerprint :: CheckGroupFingerprint
      , checkFingerprint :: CheckFingerprint
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary CheckPath where

pathForCheck :: Check -> CheckPath
pathForCheck check = CheckPath
    { pairingId = check.group.pairingId
    , groupFingerprint = check.group.fingerprint
    , checkFingerprint = check.fingerprint
    }

findCheck :: CheckPath -> Checks -> Check
findCheck path checks = theCheck
  where
    subgroup = checks ^.
        #unwrap % at path.pairingId % unwrapped % at path.groupFingerprint % unwrapped
    [theCheck] =
        [ check
        | (_i, check) <- subgroup.checks
        , check.fingerprint == path.checkFingerprint
        ]

data CheckSubgroupPath
  = CheckSubgroupPath
      { pairingId :: PairingId
      , subgroupId :: CheckSubgroupId
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary CheckSubgroupPath where

pathForCheckSubgroup :: CheckSubgroup -> CheckSubgroupPath
pathForCheckSubgroup subgroup = CheckSubgroupPath
    { pairingId = subgroup.group.pairingId
    , subgroupId = takeSubgroupId subgroup
    }

findCheckSubgroup :: CheckSubgroupPath -> Checks -> CheckSubgroup
findCheckSubgroup path checks =
    takeSubgroupByIndexInGroup (`elem` path.subgroupId.checkIndices) subgroup
  where
    subgroup = checks ^.
        #unwrap % at path.pairingId % unwrapped % at path.subgroupId.groupFingerprint % unwrapped
