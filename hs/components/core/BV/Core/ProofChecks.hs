module BV.Core.ProofChecks
    ( enumerateProofChecks
    ) where

import BV.Core.Types

enumerateProofChecks :: (PairingEqSideQuadrant -> Ident -> Ident) -> Pairing -> Problem -> ProofNode -> [ProofCheck a]
enumerateProofChecks = undefined
