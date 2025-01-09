module BV.Core.Types.Inputs where

import qualified Data.Map as M
import GHC.Generics (Generic)

import BV.Core.Types.Pairing
import BV.Core.Types.Problem
import BV.Core.Types.Program
import BV.Core.Types.ProofScript

newtype StackBounds
  = StackBounds (M.Map Ident Expr)
  deriving (Eq, Generic, Ord, Show)

newtype InlineScripts
  = InlineScripts (M.Map PairingId [InlineScriptEntry])
  deriving (Eq, Generic, Ord, Show)

type InlineScript = [InlineScriptEntry]

data InlineScriptEntry
  = InlineScriptEntry
      { nodeBySource :: NodeBySource
      , inlinedFunctionName :: Ident
      }
  deriving (Eq, Generic, Ord, Show)

newtype ProblemsAndProofs
  = ProblemsAndProofs (M.Map PairingId ProblemAndProof)
  deriving (Eq, Generic, Ord, Show)

data ProblemAndProof
  = ProblemAndProof
      { problem :: Problem
      , proof :: ProofNode
      }
  deriving (Eq, Generic, Ord, Show)
