module BV.Core.Stages.EnumerateProofChecks
    ( enumerateProofChecks
    ) where

import BV.Core.Types

enumerateProofChecks :: (PairingEqSideQuadrant -> Ident -> Ident) -> Pairing -> Problem -> ProofNode -> [ProofCheck a]
enumerateProofChecks = undefined
