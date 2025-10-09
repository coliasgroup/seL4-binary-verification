module BV.Search.Core.Solver.Common
    ( MonadGraphSliceGetSExprValue (..)
    , MonadGraphSliceSendSExpr (..)
    , MonadGraphSliceSolverInteract (..)
    ) where

import BV.Search.Core.GraphSlice (MonadGraphSliceSendSExpr (..))

import BV.Core.Types
import BV.SMTLIB2.SExpr

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)

class MonadGraphSliceSendSExpr m => MonadGraphSliceSolverInteract m where
    checkSExprHyp :: SExprWithPlaceholders -> m Bool

instance MonadGraphSliceSolverInteract m => MonadGraphSliceSolverInteract (ReaderT r m) where
    checkSExprHyp = lift . checkSExprHyp

instance MonadGraphSliceSolverInteract m => MonadGraphSliceSolverInteract (ExceptT e m) where
    checkSExprHyp = lift . checkSExprHyp

class MonadGraphSliceSolverInteract m => MonadGraphSliceGetSExprValue m where
    getSExprValue :: SExprWithPlaceholders -> m SExpr
