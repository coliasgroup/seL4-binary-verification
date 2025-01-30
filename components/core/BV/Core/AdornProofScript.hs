module BV.Core.AdornProofScript
    ( ProofScriptNodeLocation (..)
    , adornProofScriptWithDescriptions
    ) where

import BV.Core.Types

import GHC.Generics (Generic)

data ProofScriptNodeLocation
  = ProofScriptNodeLocation
  deriving (Eq, Generic, Ord, Show)

adornProofScriptWithDescriptions
    :: (ProofScriptNodeLocation -> a -> b) -> ProofScript a -> ProofScript b
adornProofScriptWithDescriptions f proofScript = undefined
