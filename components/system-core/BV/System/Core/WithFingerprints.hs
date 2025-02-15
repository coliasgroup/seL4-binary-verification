{-# LANGUAGE DeriveAnyClass #-}

module BV.System.Core.WithFingerprints
    ( FlattenedSMTProofChecksWithFingerprints (..)
    , PreparedSMTProofChecksWithFingerprints
    , SMTProofCheckGroupWithCheckFingerprints
    , SMTProofCheckGroupWithFingerprints (..)
    , SMTProofCheckMetaWithFingerprint (..)
    , SMTProofCheckSubgroupId (..)
    , SMTProofCheckSubgroupWithFingerprints (..)
    , SMTProofCheckWithFingerprint
    , SubgroupElementMeta (..)
    , decorateWithFingerprints
    , prettySMTProofCheckSubgroupIdShort
    , splitSMTProofCheckMetaWithFingerprint
    , subgroupIdOf
    , ungroupSMTProofCheckGroup
    , ungroupSMTProofCheckSubgroupWithFingerprints
    ) where

import BV.Core
import BV.System.Core.Fingerprinting

import Control.DeepSeq (NFData)
import Data.List (intercalate)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics

data SMTProofCheckMetaWithFingerprint a
  = SMTProofCheckMetaWithFingerprint
      { fingerprint :: SMTProofCheckFingerprint
      , inner :: a
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

splitSMTProofCheckMetaWithFingerprint :: SMTProofCheckMetaWithFingerprint a -> (SMTProofCheckFingerprint, a)
splitSMTProofCheckMetaWithFingerprint meta = (meta.fingerprint, meta.inner)

type SMTProofCheckWithFingerprint a = SMTProofCheck (SMTProofCheckMetaWithFingerprint a)

type SMTProofCheckGroupWithCheckFingerprints a = SMTProofCheckGroup (SMTProofCheckMetaWithFingerprint a)

data SMTProofCheckGroupWithFingerprints a
  = SMTProofCheckGroupWithFingerprint
      { fingerprint :: SMTProofCheckGroupFingerprint
      , inner :: SMTProofCheckGroupWithCheckFingerprints a
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

newtype FlattenedSMTProofChecksWithFingerprints a
  = FlattenedSMTProofChecksWithFingerprints { unwrap :: M.Map PairingId [SMTProofCheckGroupWithFingerprints a] }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving newtype (NFData)

decorateWithFingerprints :: PreparedSMTProofChecks -> PreparedSMTProofChecksWithFingerprints
decorateWithFingerprints (PreparedSMTProofChecks byPairing) =
    FlattenedSMTProofChecksWithFingerprints $
        byPairing &
            traversed % traversed %~ \group ->
                SMTProofCheckGroupWithFingerprint (fingerprintSMTProofCheckGroup group) $
                    group &
                        #imps % traversed %~ \imp -> imp &
                            #meta %~ \meta ->
                                let check = SMTProofCheck group.setup imp
                                    fingerprint = fingerprintSMTProofCheck check
                                in SMTProofCheckMetaWithFingerprint fingerprint meta

type PreparedSMTProofChecksWithFingerprints = FlattenedSMTProofChecksWithFingerprints SMTProofCheckDescription

--

data SMTProofCheckSubgroupWithFingerprints a
  = SMTProofCheckSubgroupWithFingerprints
      { groupFingerprint :: SMTProofCheckGroupFingerprint
      , inner :: SMTProofCheckGroupWithCheckFingerprints (SubgroupElementMeta a)
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data SubgroupElementMeta a
  = SubgroupElementMeta
      { indexInGroup :: Integer
      , inner :: a
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data SMTProofCheckSubgroupId
  = SMTProofCheckSubgroupId
      { groupFingerprint :: SMTProofCheckGroupFingerprint
      , checkIndices :: [Integer]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

subgroupIdOf :: SMTProofCheckSubgroupWithFingerprints a -> SMTProofCheckSubgroupId
subgroupIdOf subgroup = SMTProofCheckSubgroupId subgroup.groupFingerprint
    [ imp.meta.inner.indexInGroup
    | imp <- subgroup.inner.imps
    ]

prettySMTProofCheckSubgroupIdShort :: SMTProofCheckSubgroupId -> String
prettySMTProofCheckSubgroupIdShort subgroupId =
    prettySMTProofCheckGroupFingerprintShort subgroupId.groupFingerprint
    ++ "("
    ++ intercalate "," (map show subgroupId.checkIndices)
    ++ ")"

--

ungroupSMTProofCheckSubgroupWithFingerprints :: SMTProofCheckSubgroupWithFingerprints a -> [SMTProofCheckWithFingerprint (SubgroupElementMeta a)]
ungroupSMTProofCheckSubgroupWithFingerprints subgroup =
    subgroup.inner.imps <&> \imp -> SMTProofCheck
        { setup = subgroup.inner.setup
        , imp
        }

--

-- TODO remove
ungroupSMTProofCheckGroup :: SMTProofCheckGroup a -> [SMTProofCheck a]
ungroupSMTProofCheckGroup group = group.imps <&> \imp -> SMTProofCheck
    { setup = group.setup
    , imp
    }

--

data CheckFilter
  = CheckFilter
      { pairings :: PairingId -> Bool
      , groups :: SMTProofCheckGroupFingerprint -> Bool
      , checks :: SMTProofCheckFingerprint -> Bool
      }
  deriving (Generic)
