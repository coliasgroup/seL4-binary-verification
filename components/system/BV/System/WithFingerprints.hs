{-# LANGUAGE DeriveAnyClass #-}

module BV.System.WithFingerprints
    ( FlattenedSMTProofChecksWithFingerprints (..)
    , PreparedSMTProofChecksWithFingerprints
    , SMTProofCheckGroupWithCheckFingerprints
    , SMTProofCheckGroupWithFingerprints (..)
    , SMTProofCheckMetaWithFingerprint (..)
    , SMTProofCheckWithFingerprint
    , decorateWithFingerprints
    ) where

import BV.Core.DecorateProofScript
import BV.Core.Types
import BV.System.Fingerprinting

import Control.DeepSeq (NFData)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics

data SMTProofCheckMetaWithFingerprint a
  = SMTProofCheckMetaWithFingerprint
      { fingerprint :: SMTProofCheckFingerprint
      , inner :: a
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

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

decorateWithFingerprints :: FlattenedSMTProofChecks a -> FlattenedSMTProofChecksWithFingerprints a
decorateWithFingerprints (FlattenedSMTProofChecks byPairing) =
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

instance HasComputedFingerprint SMTProofCheckFingerprint (SMTProofCheckWithFingerprint a) where
    computedFingerprint check = check.imp.meta.fingerprint

instance HasComputedFingerprint SMTProofCheckGroupFingerprint (SMTProofCheckGroupWithFingerprints a) where
    computedFingerprint group = group.fingerprint
