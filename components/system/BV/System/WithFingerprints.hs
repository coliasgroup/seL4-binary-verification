{-# LANGUAGE DeriveAnyClass #-}

module BV.System.WithFingerprints
    ( FlattenedSMTProofChecksWithFingerprints (..)
    , PreparedSMTProofChecksWithFingerprints
    , SMTProofCheckGroupWithCheckFingerprints
    , SMTProofCheckGroupWithFingerprints (..)
    , SMTProofCheckMetaWithFingerprint (..)
    , SMTProofCheckWithFingerprint
    , adornWithFingerprints
    ) where

import BV.Core.Types
import BV.System.Fingerprinting

import BV.Core.AdornProofScript
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

adornWithFingerprints :: FlattenedSMTProofChecks a -> FlattenedSMTProofChecksWithFingerprints a
adornWithFingerprints (FlattenedSMTProofChecks byPairing) =
    FlattenedSMTProofChecksWithFingerprints $
        byPairing &
            traversed % traversed %~ \group ->
                SMTProofCheckGroupWithFingerprint (smtProofCheckGroupFingerprint group) $
                    group &
                        #imps % traversed %~ \imp -> imp &
                            #meta %~ \meta ->
                                let check = SMTProofCheck group.setup imp
                                    fingerprint = smtProofCheckFingerprint check
                                in SMTProofCheckMetaWithFingerprint fingerprint meta

type PreparedSMTProofChecksWithFingerprints = FlattenedSMTProofChecksWithFingerprints SMTProofCheckDescription

instance HasEmbeddedFingerprint SMTProofCheckFingerprint (SMTProofCheckWithFingerprint a) where
    embeddedFingerprint check = check.imp.meta.fingerprint

instance HasEmbeddedFingerprint SMTProofCheckGroupFingerprint (SMTProofCheckGroupWithFingerprints a) where
    embeddedFingerprint group = group.fingerprint
