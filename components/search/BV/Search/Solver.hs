module BV.Search.Solver
    (
    ) where

import BV.Core.RepGraph
import BV.Core.Types

class MonadRepGraphSolverSend m => MonadRepGraphSolverInteract m where
    checkHyp :: SExprWithPlaceholders -> m Bool
