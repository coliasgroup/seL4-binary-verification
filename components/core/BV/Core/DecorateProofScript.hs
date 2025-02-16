{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.DecorateProofScript
    ( ProofScriptNodePath (..)
    , decorateProofScriptWithProofScriptNodePathsWith
    , prettyProofScriptNodePath
    ) where

import BV.Core.Types

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Optics

-- TODO implement
data ProofScriptNodePath
  = ProofScriptNodePath
  deriving (Eq, Generic, NFData, Ord, Show)

-- TODO implement
decorateProofScriptWithProofScriptNodePathsWith
    :: (ProofScriptNodePath -> a -> b) -> ProofScript a -> ProofScript b
decorateProofScriptWithProofScriptNodePathsWith f = #root % traversed %~ f ProofScriptNodePath

-- TODO implement
prettyProofScriptNodePath :: ProofScriptNodePath -> String
prettyProofScriptNodePath _loc = "TODO\n"
