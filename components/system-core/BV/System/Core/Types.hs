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
    , indexSubgroup
    , pathForCheck
    , pathForCheckSubgroup
    , prettyCheckSubgroupIdShort
    , prettyProofCheckMeta
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
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (genericIndex, genericSplitAt, intercalate, nubBy)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

data Check
  = Check
      { fingerprint :: CheckFingerprint
      , group :: CheckGroup
      , indexInGroup :: CheckIndexInGroup
      , desc :: ProofCheckDescription
      , imp :: SExprWithPlaceholders
      }
  deriving (Eq, Generic, Ord, Show)

data CheckGroup
  = CheckGroup
      { fingerprint :: CheckGroupFingerprint
      , path :: CheckGroupPath
      , proofScriptNodePath :: ProofScriptNodePath AsmRefineTag
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
      , checks :: M.Map CheckIndexInGroup Check
      }
  deriving (Eq, Generic, Ord, Show)

data CheckSubgroupId
  = CheckSubgroupId
      { groupFingerprint :: CheckGroupFingerprint
      , checkIndices :: S.Set CheckIndexInGroup
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary CheckSubgroupId

newtype Checks
  = Checks { unwrap :: M.Map PairingId' (M.Map ProofScriptEdgePath (M.Map ProofCheckGroupCheckIndices CheckSubgroup)) }
  deriving (Eq, Generic, Ord, Show)

elaborateChecksFromInput :: StagesInput -> Checks
elaborateChecksFromInput input = elaborateChecks (stages input).checks

elaborateChecks :: SMTProofChecks' -> Checks
elaborateChecks (SMTProofChecks smtProofChecks) =
    Checks $
        flip M.mapWithKey smtProofChecks $ \pairingId ->
            M.fromList . toList .
                (decorateProofScriptWithProofScriptNodePathsWith $ \nodePath (groups :: [(ProofCheckGroupCheckIndices, SMTProofCheckGroup ProofCheckDescription)]) ->
                    let edgePath = proofScriptEdgePath nodePath
                        g indices_ smtCheckGroup =
                            let group = CheckGroup
                                    { fingerprint = fingerprintCheckGroup smtCheckGroup
                                    , path = CheckGroupPath
                                        { pairingId
                                        , proofScriptEdgePath = edgePath
                                        , checkIndices = indices_
                                        , fingerprint = group.fingerprint
                                        }
                                    , proofScriptNodePath = nodePath
                                    , setup = smtCheckGroup.setup
                                    , checks =
                                        [ let fingerprint = fingerprintCheck (SMTProofCheck smtCheckGroup.setup imp)
                                           in Check
                                                { fingerprint
                                                , group
                                                , indexInGroup
                                                , desc = imp.meta
                                                , imp = imp.term
                                                }
                                        | (indexInGroup, imp) <- zip (map CheckIndexInGroup [0..]) smtCheckGroup.imps
                                        ]
                                    }
                             in fullSubgroup group
                     in ( proofScriptEdgePath nodePath
                        , M.mapWithKey g $ M.fromList groups
                        ))

fullSubgroup :: CheckGroup -> CheckSubgroup
fullSubgroup group = CheckSubgroup
    { group
    , checks = M.fromList $ zip (map CheckIndexInGroup [0..]) group.checks
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
    f :: PairingId' -> M.Map ProofCheckGroupCheckIndices CheckSubgroup -> M.Map ProofCheckGroupCheckIndices CheckSubgroup
    f pairingId = case (checkFilter.groups pairingId, checkFilter.checks pairingId) of
        (Nothing, Nothing) -> id
        (fgroupOpt, fcheckOpt) ->
            let fgroup = fromMaybe (const True) fgroupOpt
                fcheck = fromMaybe (const True) fcheckOpt
             in (traversed %~ takeSubgroupByFingerprint fcheck)
                    . M.filter (\v -> fgroup v.group.fingerprint)

takeEmptySubgroup :: CheckSubgroup -> CheckSubgroup
takeEmptySubgroup = #checks .~ M.empty

takeSubgroupByIndexInGroup :: (CheckIndexInGroup -> Bool) -> CheckSubgroup -> CheckSubgroup
takeSubgroupByIndexInGroup p = #checks %~ M.filterWithKey (\i _check -> p i)

takeSubgroupByFingerprint :: (CheckFingerprint -> Bool) -> CheckSubgroup -> CheckSubgroup
takeSubgroupByFingerprint p = #checks %~ M.filter (\check -> p check.fingerprint)

splitSubgroupAt :: Integer -> CheckSubgroup -> (CheckSubgroup, CheckSubgroup)
splitSubgroupAt i subgroup = (subgroup & #checks .~ l, subgroup & #checks .~ r)
  where
    (l, r) = over each M.fromList $ genericSplitAt i $ M.toList subgroup.checks

indexSubgroup :: CheckSubgroup -> Integer -> Check
indexSubgroup subgroup i = toList subgroup.checks `genericIndex` i

deduplicateSubgroup :: CheckSubgroup -> CheckSubgroup
deduplicateSubgroup = #checks %~ M.fromList . nubBy ((==) `on` view (_2 % #fingerprint)) . M.toList

takeSubgroupId :: CheckSubgroup -> CheckSubgroupId
takeSubgroupId subgroup = CheckSubgroupId
    { groupFingerprint = subgroup.group.fingerprint
    , checkIndices = M.keysSet subgroup.checks
    }

prettyCheckSubgroupIdShort :: CheckSubgroupId -> String
prettyCheckSubgroupIdShort subgroupId =
    prettyCheckGroupFingerprintShort subgroupId.groupFingerprint
    ++ "("
    ++ intercalate "," (map (show . (.unwrap)) (toList subgroupId.checkIndices))
    ++ ")"

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
        | check <- toList subgroup.checks
        ]
    }

data CheckPath
  = CheckPath
      { checkGroupPath :: CheckGroupPath
      , indexInGroup :: CheckIndexInGroup
      , fingerprint :: CheckFingerprint
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary CheckPath

pathForCheck :: Check -> CheckPath
pathForCheck check = CheckPath
    { checkGroupPath = check.group.path
    , indexInGroup = check.indexInGroup
    , fingerprint = check.fingerprint
    }

findCheck :: CheckPath -> Checks -> Check
findCheck path checks = findCheckGroup path.checkGroupPath checks
    ^. #checks
    % expectingAt path.indexInGroup

data CheckGroupPath
  = CheckGroupPath
      { pairingId :: PairingId'
      , proofScriptEdgePath :: ProofScriptEdgePath
      , checkIndices :: ProofCheckGroupCheckIndices
      , fingerprint :: CheckGroupFingerprint
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary CheckGroupPath

findCheckGroup :: CheckGroupPath -> Checks -> CheckSubgroup
findCheckGroup path checks = checks
    ^. #unwrap
    % expectingAt path.pairingId
    % expectingAt path.proofScriptEdgePath
    % expectingAt path.checkIndices

data CheckSubgroupPath
  = CheckSubgroupPath
      { groupPath :: CheckGroupPath
      , checkIndices :: S.Set CheckIndexInGroup
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary CheckSubgroupPath

pathForCheckSubgroup :: CheckSubgroup -> CheckSubgroupPath
pathForCheckSubgroup subgroup = CheckSubgroupPath
    { groupPath = subgroup.group.path
    , checkIndices = M.keysSet subgroup.checks
    }

findCheckSubgroup :: CheckSubgroupPath -> Checks -> CheckSubgroup
findCheckSubgroup path checks =
    takeSubgroupByIndexInGroup (`S.member` path.checkIndices) $
        findCheckGroup path.groupPath checks

--

prettyProofCheckMeta :: Check -> String
prettyProofCheckMeta check =
    prettyProofScriptNodePath check.group.proofScriptNodePath ++ " >>> " ++ check.desc
