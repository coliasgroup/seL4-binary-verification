{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

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
    , ChecksForPairing (..)
    , deduplicateSubgroup
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

import BV.Core.Prelude
import BV.System.Core.Fingerprinting
import BV.Utils (unwrapped)

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Function (on)
import Data.List (genericLength, genericSplitAt, intercalate, nubBy)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Optics

data Check
  = Check
      { fingerprint :: CheckFingerprint
      , group :: CheckGroup
      , meta :: ProofCheckMeta
      , imp :: SExprWithPlaceholders
      }
  deriving (Eq, Generic, Ord, Show)

data CheckGroup
  = CheckGroup
      { fingerprint :: CheckGroupFingerprint
      , pairingId :: PairingId'
      , setup :: [SExprWithPlaceholders]
      , checks :: [Check]
      }
  deriving (Eq, Generic, Ord, Show)

newtype CheckIndexInGroup
  = CheckIndexInGroup { unwrap :: Integer }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (Binary, NFData)

data CheckSubgroup
  = CheckSubgroup
      { group :: CheckGroup
      , checks :: [(CheckIndexInGroup, Check)]
      }
  deriving (Eq, Generic, Ord, Show)

data CheckSubgroupId
  = CheckSubgroupId
      { groupFingerprint :: CheckGroupFingerprint
      , checkIndices :: [CheckIndexInGroup]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary CheckSubgroupId

newtype Checks
  = Checks { unwrap :: M.Map PairingId' ChecksForPairing }
  deriving (Eq, Generic, Ord, Show)

data ChecksForPairing
  = ChecksForPairing
      { groups :: M.Map CheckGroupFingerprint CheckSubgroup
      , count :: Integer
        -- compute this before creating map, to enable laziness
      }
  deriving (Eq, Generic, Ord, Show)

elaborateChecksFromInput :: StagesInput -> Checks
elaborateChecksFromInput input = elaborateChecks (stages input).checks

elaborateChecks :: StagesOutputChecks -> Checks
elaborateChecks stagesOutputChecks = Checks $ M.mapWithKey f stagesOutputChecks.unwrap
  where
    f pairingId stagesOutputGroups = ChecksForPairing
        { count
        , groups = M.fromListWith (error "unexpected") groups
        }
      where
        count = genericLength groups
        groups =
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
      { pairings :: PairingId' -> Bool
      , groups :: PairingId' -> Maybe (CheckGroupFingerprint -> Bool)
      , checks :: PairingId' -> Maybe (CheckFingerprint -> Bool)
      }
  deriving (Generic)

filterChecks :: CheckFilter -> Checks -> Checks
filterChecks checkFilter = #unwrap %~ ((iover itraversed f) . M.filterWithKey (\k _v -> checkFilter.pairings k))
  where
    f :: PairingId' -> ChecksForPairing -> ChecksForPairing
    f pairingId checksForPairing = case (checkFilter.groups pairingId, checkFilter.checks pairingId) of
        (Nothing, Nothing) -> checksForPairing
        (fgroup, fcheck) -> ChecksForPairing
            { groups = groups
            , count = toInteger $ M.size groups
            }
          where
            groups = checksForPairing.groups &
                (traversed %~ takeSubgroupByFingerprint (fromMaybe (const True) fcheck))
                    . M.filterWithKey (\k _v -> fromMaybe (const True) fgroup k)

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

deduplicateSubgroup :: CheckSubgroup -> CheckSubgroup
deduplicateSubgroup = #checks %~ nubBy ((==) `on` view (_2 % #fingerprint))

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
      { pairingId :: PairingId'
      , groupFingerprint :: CheckGroupFingerprint
      , checkFingerprint :: CheckFingerprint
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary CheckPath

pathForCheck :: Check -> CheckPath
pathForCheck check = CheckPath
    { pairingId = check.group.pairingId
    , groupFingerprint = check.group.fingerprint
    , checkFingerprint = check.fingerprint
    }

findChecks :: CheckPath -> Checks -> [Check]
findChecks path checks =
    [ check
    | (_i, check) <- subgroup.checks
    , check.fingerprint == path.checkFingerprint
    ]
  where
    subgroup = checks ^.
        #unwrap % at path.pairingId % unwrapped % #groups % at path.groupFingerprint % unwrapped

findCheck :: CheckPath -> Checks -> Check
findCheck path checks = head $ findChecks path checks

data CheckSubgroupPath
  = CheckSubgroupPath
      { pairingId :: PairingId'
      , subgroupId :: CheckSubgroupId
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary CheckSubgroupPath

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
        #unwrap % at path.pairingId % unwrapped % #groups % at path.subgroupId.groupFingerprint % unwrapped
