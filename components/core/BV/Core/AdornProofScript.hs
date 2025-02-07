{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.AdornProofScript
    ( ProofScriptNodeLocation (..)
    , SMTProofCheckDescription (..)
    , adornProofScriptWithProofScriptNodeLocationsWith
    , adornSMTProofChecksWithDescriptions
    , prettyProofScriptNodeLocation
    , prettySMTProofCheckDescription
    ) where

import BV.Core.Types

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Optics

data ProofScriptNodeLocation
  = ProofScriptNodeLocation
  deriving (Eq, Generic, NFData, Ord, Show)

adornProofScriptWithProofScriptNodeLocationsWith
    :: (ProofScriptNodeLocation -> a -> b) -> ProofScript a -> ProofScript b
adornProofScriptWithProofScriptNodeLocationsWith f = #root % traversed %~ f ProofScriptNodeLocation

data SMTProofCheckDescription
  = SMTProofCheckDescription
      { proofScriptNodeLocation :: ProofScriptNodeLocation
      , meta :: String
      }
  deriving (Eq, Generic, NFData, Ord, Show)

adornSMTProofChecksWithDescriptions
    :: SMTProofChecks String -> SMTProofChecks SMTProofCheckDescription
adornSMTProofChecksWithDescriptions = #unwrap % traversed %~
    adornProofScriptWithProofScriptNodeLocationsWith (map . fmap . SMTProofCheckDescription)

prettySMTProofCheckDescription :: SMTProofCheckDescription -> String
prettySMTProofCheckDescription desc =
    desc.meta <> " at:\n" <> prettyProofScriptNodeLocation desc.proofScriptNodeLocation

prettyProofScriptNodeLocation :: ProofScriptNodeLocation -> String
prettyProofScriptNodeLocation _loc = "TODO\n"
