module BV.Core.Stages.CompileProofChecks
    ( compileProofChecks
    ) where

import BV.Core.Types

compileProofChecks :: (PairingEqSideQuadrant -> Ident -> Ident) -> Pairing -> Problem -> [ProofCheck a] -> [SmtProofCheckGroup a]
compileProofChecks = undefined
