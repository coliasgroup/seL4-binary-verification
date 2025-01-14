module BV.SMTLIB2.Types
    ( module BV.SMTLIB2.Types.SExpr
    , MonadSolver (..)
    , readAtom
    , readSExpr
    , readSExprs
    , tryReadAtom
    , tryReadSExpr
    , tryReadSExprs
    ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift)

import BV.SMTLIB2.Types.SExpr
import BV.SMTLIB2.Types.SExpr.Read

class Monad m => MonadSolver m where
    send :: SExpr -> m ()
    recv :: m SExpr

instance MonadSolver m => MonadSolver (ExceptT e m) where
    send = lift . send
    recv = lift recv
