{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.SearchOutputs
    ( InlineScript
    , InlineScriptEntry (..)
    , InlineScripts (..)
    , Proofs (..)
    , StackBounds (..)
    ) where

import BV.Core.Types.Pairing
import BV.Core.Types.Problem
import BV.Core.Types.Program
import BV.Core.Types.ProofScript

import Control.DeepSeq (NFData)
import qualified Data.Map as M
import GHC.Generics (Generic)

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

newtype Proofs a
  = Proofs { unwrap :: M.Map PairingId (ProofScript a) }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance AtPairingId (ProofScript a) (Proofs a) where
    atPairingId = atPairingId . (.unwrap)
