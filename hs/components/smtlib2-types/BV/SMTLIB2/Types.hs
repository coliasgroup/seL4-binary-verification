module BV.SMTLIB2.Types
    ( module BV.SMTLIB2.Types.SExpr
    , MonadSolver (..)
    ) where

import BV.SMTLIB2.Types.SExpr

class Monad m => MonadSolver m where
    interact :: SExpr -> m SExpr
