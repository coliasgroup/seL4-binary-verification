module BV.Core.Types.Extras.Problem
    ( varNamesOfProblem
    ) where

import BV.Core.Types
import BV.Core.Types.Extras.Program
import Optics

varNamesOfProblem :: Traversal' Problem Ident
varNamesOfProblem =
    (#sides % traversed % (#input `adjoin` #output) % traversed % varNamesOf)
        `adjoin` (#nodes % traversed % varNamesOf)
