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
import Data.Binary (Binary)
import Data.Foldable (fold)
import qualified Data.Map as M
import GHC.Generics (Generic)

newtype CompatProofChecks
  = CompatProofChecks { unwrap :: M.Map PairingId [ProofCheck String] }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

toCompatProofChecks :: ProofChecks String -> CompatProofChecks
toCompatProofChecks (ProofChecks byPairing) = CompatProofChecks (M.map fold byPairing)

newtype CompatSMTProofChecks
  = CompatSMTProofChecks { unwrap :: M.Map PairingId [SMTProofCheckGroup ()] }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance Binary CompatSMTProofChecks where

toCompatSMTProofChecks :: SMTProofChecks () -> CompatSMTProofChecks
toCompatSMTProofChecks (SMTProofChecks byPairing) = CompatSMTProofChecks (M.map fold byPairing)
