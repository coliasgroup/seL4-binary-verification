module BV.Core.AdornProofScript
    ( ProofScriptNodeLocation (..)
    , SMTProofCheckDescription (..)
    , adornProofScriptWithProofScriptNodeLocationsWith
    , adornSMTProofChecksWithDescriptions
    ) where

import BV.Core.Types

import GHC.Generics (Generic)
import Optics

data ProofScriptNodeLocation
  = ProofScriptNodeLocation
  deriving (Eq, Generic, Ord, Show)

adornProofScriptWithProofScriptNodeLocationsWith
    :: (ProofScriptNodeLocation -> a -> b) -> ProofScript a -> ProofScript b
adornProofScriptWithProofScriptNodeLocationsWith f proofScript = undefined

data SMTProofCheckDescription a
  = SMTProofCheckDescription
      { proofScriptNodeLocation :: ProofScriptNodeLocation
      , meta :: a
      }
  deriving (Eq, Generic, Ord, Show)

adornSMTProofChecksWithDescriptions
    :: SMTProofChecks a -> SMTProofChecks (SMTProofCheckDescription a)
adornSMTProofChecksWithDescriptions = #unwrap % traversed %~
    adornProofScriptWithProofScriptNodeLocationsWith (map . fmap . SMTProofCheckDescription)
