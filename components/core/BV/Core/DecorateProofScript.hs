{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.DecorateProofScript
    ( ProofScriptNodeLocation (..)
    , SMTProofCheckDescription (..)
    , decorateProofScriptWithProofScriptNodeLocationsWith
    , decorateSMTProofChecksWithDescriptions
    , prettyProofScriptNodeLocation
    , prettySMTProofCheckDescription
    ) where

import BV.Core.Types

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Optics

-- TODO implement
data ProofScriptNodeLocation
  = ProofScriptNodeLocation
  deriving (Eq, Generic, NFData, Ord, Show)

-- TODO implement
decorateProofScriptWithProofScriptNodeLocationsWith
    :: (ProofScriptNodeLocation -> a -> b) -> ProofScript a -> ProofScript b
decorateProofScriptWithProofScriptNodeLocationsWith f = #root % traversed %~ f ProofScriptNodeLocation

data SMTProofCheckDescription
  = SMTProofCheckDescription
      { proofScriptNodeLocation :: ProofScriptNodeLocation
      , meta :: String
      }
  deriving (Eq, Generic, NFData, Ord, Show)

decorateSMTProofChecksWithDescriptions
    :: SMTProofChecks String -> SMTProofChecks SMTProofCheckDescription
decorateSMTProofChecksWithDescriptions = #unwrap % traversed %~
    decorateProofScriptWithProofScriptNodeLocationsWith (map . fmap . SMTProofCheckDescription)

prettySMTProofCheckDescription :: SMTProofCheckDescription -> String
prettySMTProofCheckDescription desc =
    desc.meta <> " at:\n" <> prettyProofScriptNodeLocation desc.proofScriptNodeLocation

prettyProofScriptNodeLocation :: ProofScriptNodeLocation -> String
prettyProofScriptNodeLocation _loc = "TODO\n"
