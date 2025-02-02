module BV.Core.Types.Compat
    ( CompatProofChecks (..)
    , CompatSMTProofChecks (..)
    , toCompatProofChecks
    , toCompatSMTProofChecks
    ) where

import BV.Core.Types.Pairing
import BV.Core.Types.ProofChecks
import BV.Core.Types.SMTProofChecks

import Control.DeepSeq (NFData)
import Data.Foldable (fold)
import qualified Data.Map as M
import GHC.Generics (Generic)

newtype CompatProofChecks a
  = CompatProofChecks { unwrap :: M.Map PairingId [ProofCheck a] }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving newtype (NFData)

toCompatProofChecks :: ProofChecks a -> CompatProofChecks a
toCompatProofChecks (ProofChecks byPairing) = CompatProofChecks (M.map fold byPairing)

newtype CompatSMTProofChecks a
  = CompatSMTProofChecks { unwrap :: M.Map PairingId [SMTProofCheckGroup a] }
  deriving (Eq, Functor, Generic, Ord, Show)
  deriving newtype (NFData)

toCompatSMTProofChecks :: SMTProofChecks a -> CompatSMTProofChecks a
toCompatSMTProofChecks (SMTProofChecks byPairing) = CompatSMTProofChecks (M.map fold byPairing)
