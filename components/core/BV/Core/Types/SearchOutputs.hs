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

newtype InlineScripts t
  = InlineScripts { unwrap :: M.Map (PairingId t) (InlineScript t) }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance (Tag t, Binary t) => Binary (InlineScripts t) where

type InlineScript t = [InlineScriptEntry t]

data InlineScriptEntry t
  = InlineScriptEntry
      { nodeBySource :: (NodeBySource t)
      , inlinedFunctionName :: Ident
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary t => Binary (InlineScriptEntry t) where

newtype Proofs t a
  = Proofs { unwrap :: M.Map (PairingId t) (ProofScript t a) }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance (Tag t, Binary t, Binary a) => Binary (Proofs t a) where
