module BV.Search.Core.Solver.Old
    ( module BV.Search.Core.Solver.Common
    , testHyp
    ) where

import BV.Core.GraphSlice.Old

import BV.Search.Core.Solver.Common

import BV.Core.Types

import Control.Monad.Trans (lift)

testHyp :: (Tag t, MonadGraphSliceSolverInteract m) => FlatExpr -> GraphSliceT t m Bool
testHyp expr = do
    sexpr <- convertExpr expr
    addAccumulatedAssertions
    lift $ checkSExprHyp sexpr
