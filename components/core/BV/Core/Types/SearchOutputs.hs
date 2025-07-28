{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.SearchOutputs
    ( InlineScript
    , InlineScript'
    , InlineScriptEntry (..)
    , InlineScripts (..)
    , Proofs (..)
    , StackBounds (..)
    ) where

import BV.Core.Types.Pairing
import BV.Core.Types.Problem
import BV.Core.Types.Program
import BV.Core.Types.ProofScript
import BV.Core.Types.Tag

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import qualified Data.Map as M
import GHC.Generics (Generic)

newtype StackBounds
  = StackBounds { unwrap :: M.Map Ident Expr }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance Binary StackBounds where

newtype InlineScripts
  = InlineScripts { unwrap :: M.Map PairingId' InlineScript' }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance Binary InlineScripts where

instance AtPairingId AsmRefineTag (InlineScript AsmRefineTag) InlineScripts where
    atPairingId = atPairingId . (.unwrap)

type InlineScript' = InlineScript AsmRefineTag

type InlineScript t = [InlineScriptEntry t]

data InlineScriptEntry t
  = InlineScriptEntry
      { nodeBySource :: (NodeBySource t)
      , inlinedFunctionName :: Ident
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary t => Binary (InlineScriptEntry t) where

newtype Proofs a
  = Proofs { unwrap :: M.Map PairingId' (ProofScript AsmRefineTag a) }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance Binary a => Binary (Proofs a) where

instance AtPairingId AsmRefineTag (ProofScript AsmRefineTag a) (Proofs a) where
    atPairingId = atPairingId . (.unwrap)
