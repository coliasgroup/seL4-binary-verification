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
import BV.Utils (expectingAt)

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Function (on)
import Data.List (genericSplitAt, intercalate, nubBy)
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
      , proofScriptNode :: ProofScriptEdgePath
      , groupIndices :: ProofCheckGroupIndices
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
  = Checks { unwrap :: M.Map PairingId' (M.Map ProofScriptEdgePath (M.Map ProofCheckGroupIndices CheckSubgroup)) }
  deriving (Eq, Generic, Ord, Show)

elaborateChecksFromInput :: StagesInput -> Checks
elaborateChecksFromInput input = elaborateChecks (stages input).checks

elaborateChecks :: StagesOutputChecks -> Checks
elaborateChecks stagesOutputChecks = Checks $
    flip M.mapWithKey stagesOutputChecks.unwrap $ \pairingId ->
        M.mapWithKey $ \proofScriptNode ->
            M.mapWithKey $ \groupIndices smtGroup ->
                let group = CheckGroup
                        { fingerprint = fingerprintCheckGroup smtGroup
                        , pairingId
                        , proofScriptNode
                        , groupIndices
                        , setup = smtGroup.setup
                        , checks =
                            [ Check
                                { fingerprint = fingerprintCheck (SMTProofCheck smtGroup.setup imp)
                                , group
                                , meta = imp.meta
                                , imp = imp.term
                                }
                            | imp <- smtGroup.imps
                            ]
                        }
                 in fullSubgroup group

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
filterChecks checkFilter = #unwrap %~ (iover (itraversed % traversed) f . M.filterWithKey (\k _v -> checkFilter.pairings k))
  where
    f :: PairingId' -> M.Map ProofCheckGroupIndices CheckSubgroup -> M.Map ProofCheckGroupIndices CheckSubgroup
    f pairingId xgroups = case (checkFilter.groups pairingId, checkFilter.checks pairingId) of
        (Nothing, Nothing) -> xgroups
        (fgroup, fcheck) -> groups
          where
            groups = xgroups &
                (traversed %~ takeSubgroupByFingerprint (fromMaybe (const True) fcheck))
                    . M.filter (\v -> fromMaybe (const True) fgroup v.group.fingerprint)

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
      , proofScriptNode :: ProofScriptEdgePath
      , groupIndices :: ProofCheckGroupIndices
      , groupFingerprint :: CheckGroupFingerprint
      , checkFingerprint :: CheckFingerprint
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary CheckPath

pathForCheck :: Check -> CheckPath
pathForCheck check = CheckPath
    { pairingId = check.group.pairingId
    , proofScriptNode = check.group.proofScriptNode
    , groupIndices = check.group.groupIndices
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
        #unwrap
            % expectingAt path.pairingId
            % expectingAt path.proofScriptNode
            % expectingAt path.groupIndices

findCheck :: CheckPath -> Checks -> Check
findCheck path checks = head $ findChecks path checks

data CheckSubgroupPath
  = CheckSubgroupPath
      { pairingId :: PairingId'
      , proofScriptNode :: ProofScriptEdgePath
      , groupIndices :: ProofCheckGroupIndices
      , subgroupId :: CheckSubgroupId
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary CheckSubgroupPath

pathForCheckSubgroup :: CheckSubgroup -> CheckSubgroupPath
pathForCheckSubgroup subgroup = CheckSubgroupPath
    { pairingId = subgroup.group.pairingId
    , proofScriptNode = subgroup.group.proofScriptNode
    , groupIndices = subgroup.group.groupIndices
    , subgroupId = takeSubgroupId subgroup
    }

findCheckSubgroup :: CheckSubgroupPath -> Checks -> CheckSubgroup
findCheckSubgroup path checks =
    takeSubgroupByIndexInGroup (`elem` path.subgroupId.checkIndices) subgroup
  where
    subgroup = checks ^.
        #unwrap
            % expectingAt path.pairingId
            % expectingAt path.proofScriptNode
            % expectingAt path.groupIndices
