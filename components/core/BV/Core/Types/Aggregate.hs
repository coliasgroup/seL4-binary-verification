module BV.Core.Types.Aggregate
    ( CompatProofChecks (..)
    , CompatSMTProofChecks (..)
    , InlineScripts (..)
    , InlineScripts'
    , Pairings (..)
    , Pairings'
    , Problems (..)
    , Problems'
    , ProofChecks (..)
    , ProofChecks'
    , ProofScripts (..)
    , ProofScripts'
    , SMTProofChecks (..)
    , SMTProofChecks'
    , StackBounds (..)
    , toCompatProofChecks
    , toCompatSMTProofChecks
    ) where

import BV.Core.Types.AsmRefineTag
import BV.Core.Types.Pairing
import BV.Core.Types.Problem
import BV.Core.Types.Program
import BV.Core.Types.ProofCheck
import BV.Core.Types.ProofScript
import BV.Core.Types.SMTProofChecks
import BV.Core.Types.Tag

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Foldable (fold)
import Data.Functor (void)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics

-- stages

newtype Pairings t
  = Pairings { unwrap :: M.Map (PairingId t) (Pairing t) }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

type Pairings' = Pairings AsmRefineTag

instance Semigroup (Pairings t) where
    x <> y = Pairings (x.unwrap <> y.unwrap)

instance Monoid (Pairings t) where
    mempty = Pairings mempty

newtype Problems t
  = Problems { unwrap :: M.Map (PairingId t) (Problem t) }
  deriving (Eq, Generic)
  deriving newtype (NFData)

type Problems' = Problems AsmRefineTag

newtype ProofChecks t a
  = ProofChecks { unwrap :: M.Map (PairingId t) (ProofScript t (ProofCheckGroup t a)) }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving newtype (NFData)

type ProofChecks' = ProofChecks AsmRefineTag ProofCheckDescription

newtype SMTProofChecks t a
  = SMTProofChecks { unwrap :: M.Map (PairingId t) (ProofScript t [(ProofCheckGroupCheckIndices, SMTProofCheckGroup a)]) }
  deriving (Eq, Functor, Generic, Ord, Show)
  deriving newtype (NFData)

type SMTProofChecks' = SMTProofChecks AsmRefineTag ProofCheckDescription

-- compat

newtype CompatProofChecks
  = CompatProofChecks { unwrap :: M.Map PairingId' (ProofCheckGroup AsmRefineTag String) }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

toCompatProofChecks :: ProofChecks AsmRefineTag String -> CompatProofChecks
toCompatProofChecks (ProofChecks byPairing) = CompatProofChecks $ M.map fold byPairing

newtype CompatSMTProofChecks
  = CompatSMTProofChecks { unwrap :: M.Map PairingId' [SMTProofCheckGroup ()] }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

toCompatSMTProofChecks :: SMTProofChecks AsmRefineTag a -> CompatSMTProofChecks
toCompatSMTProofChecks smtProofChecks = CompatSMTProofChecks $
    M.map (toListOf (folded % folded % _2)) (void smtProofChecks).unwrap

-- search outputs

newtype StackBounds
  = StackBounds { unwrap :: M.Map Ident Expr }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance Binary StackBounds

newtype InlineScripts t
  = InlineScripts { unwrap :: M.Map (PairingId t) (InlineScript t) }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance (Tag t, Binary t) => Binary (InlineScripts t)

type InlineScripts' = InlineScripts AsmRefineTag

newtype ProofScripts t a
  = ProofScripts { unwrap :: M.Map (PairingId t) (ProofScript t a) }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance (Tag t, Binary t, Binary a) => Binary (ProofScripts t a)

type ProofScripts' = ProofScripts AsmRefineTag ()
