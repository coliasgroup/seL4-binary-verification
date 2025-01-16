module BV.Core.Stages.FormulatePairing
    ( formulatePairing
    ) where

import BV.Core.Types

formulatePairing :: Expr -> [Argument] -> [Argument] -> Pairing
formulatePairing stackBound cInput cOutpuit = Pairing { inEqs, outEqs }
  where
    inEqs = []
    outEqs = []
