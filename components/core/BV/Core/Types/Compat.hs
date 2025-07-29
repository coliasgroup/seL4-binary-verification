module BV.Core.Types.Compat
    ( CompatProofChecks (..)
    , CompatSMTProofChecks (..)
    , toCompatProofChecks
    , toCompatSMTProofChecks
    ) where

import BV.Core.Types.AsmRefineTag
import BV.Core.Types.ProofChecks
import BV.Core.Types.SMTProofChecks

import Control.DeepSeq (NFData)
import Data.Foldable (fold)
import qualified Data.Map as M
import GHC.Generics (Generic)

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
