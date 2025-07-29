module BV.Core.Types.Aggregate
    ( CompatProofChecks (..)
    , CompatSMTProofChecks (..)
    , InlineScripts (..)
    , Problems' (..)
    , ProofChecks (..)
    , Proofs (..)
    , SMTProofChecks (..)
    , StackBounds (..)
    , toCompatProofChecks
    , toCompatSMTProofChecks
    ) where

import BV.Core.Types.AsmRefineTag
import BV.Core.Types.Pairing
import BV.Core.Types.Problem
import BV.Core.Types.Program
import BV.Core.Types.ProofChecks
import BV.Core.Types.ProofScript
import BV.Core.Types.SMTProofChecks
import BV.Core.Types.Tag

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Foldable (fold)
import qualified Data.Map as M
import GHC.Generics (Generic)

newtype Problems'
  = Problems' { unwrap :: M.Map PairingId' Problem' }
  deriving (Eq, Generic)
  deriving newtype (NFData)

newtype ProofChecks t a
  = ProofChecks { unwrap :: M.Map (PairingId t) (ProofScript t [ProofCheck t a]) }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving newtype (NFData)

newtype SMTProofChecks t a
  = SMTProofChecks { unwrap :: M.Map (PairingId t) (ProofScript t [SMTProofCheckGroup a]) }
  deriving (Eq, Functor, Generic, Ord, Show)
  deriving newtype (NFData)

-- compat

newtype CompatProofChecks
  = CompatProofChecks { unwrap :: M.Map PairingId' [ProofCheck AsmRefineTag String] }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

toCompatProofChecks :: ProofChecks AsmRefineTag String -> CompatProofChecks
toCompatProofChecks (ProofChecks byPairing) = CompatProofChecks (M.map fold byPairing)

newtype CompatSMTProofChecks
  = CompatSMTProofChecks { unwrap :: M.Map PairingId' [SMTProofCheckGroup ()] }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

toCompatSMTProofChecks :: SMTProofChecks AsmRefineTag () -> CompatSMTProofChecks
toCompatSMTProofChecks (SMTProofChecks byPairing) = CompatSMTProofChecks (M.map fold byPairing)

-- search outputs

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

newtype Proofs t a
  = Proofs { unwrap :: M.Map (PairingId t) (ProofScript t a) }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance (Tag t, Binary t, Binary a) => Binary (Proofs t a) where
