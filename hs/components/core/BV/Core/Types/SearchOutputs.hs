{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.SearchOutputs where

import Control.DeepSeq (NFData)
import qualified Data.Map as M
import GHC.Generics (Generic)

import BV.Core.Types.Pairing
import BV.Core.Types.Problem
import BV.Core.Types.Program
import BV.Core.Types.ProofScript

newtype StackBounds
  = StackBounds { unwrap :: M.Map Ident Expr }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

newtype InlineScripts
  = InlineScripts { unwrap :: M.Map PairingId InlineScript }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance AtPairingId InlineScript InlineScripts where
    atPairingId = atPairingId . (.unwrap)

type InlineScript = [InlineScriptEntry]

data InlineScriptEntry
  = InlineScriptEntry
      { nodeBySource :: NodeBySource
      , inlinedFunctionName :: Ident
      }
  deriving (Eq, Generic, NFData, Ord, Show)

newtype ProblemsAndProofs
  = ProblemsAndProofs { unwrap :: M.Map PairingId ProblemAndProof }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance AtPairingId ProblemAndProof ProblemsAndProofs where
    atPairingId = atPairingId . (.unwrap)

data ProblemAndProof
  = ProblemAndProof
      { problem :: Problem
      , proof :: ProofScript
      }
  deriving (Eq, Generic, NFData, Ord, Show)
