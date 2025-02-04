module BV.SMTLIB2
    ( module BV.SMTLIB2.SExpr
    , MonadSolver (..)
    , SolverTimeout (..)
    , readAtom
    , readSExpr
    , readSExprs
    , recv
    , tryReadAtom
    , tryReadSExpr
    , tryReadSExprs
    ) where

import BV.SMTLIB2.SExpr
import BV.SMTLIB2.SExpr.Read

import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)

newtype SolverTimeout
  = SolverTimeout { seconds :: Integer }
  deriving (Eq, Generic, Ord, Show)

class Monad m => MonadSolver m where
    send :: SExpr -> m ()
    recvWithTimeout :: Maybe SolverTimeout -> m (Maybe SExpr)

recv :: MonadSolver m => m SExpr
recv = fromJust <$> recvWithTimeout Nothing

instance MonadSolver m => MonadSolver (ExceptT e m) where
    send = lift . send
    recvWithTimeout = lift . recvWithTimeout

instance MonadSolver m => MonadSolver (StateT s m) where
    send = lift . send
    recvWithTimeout = lift . recvWithTimeout

instance (Monoid w, MonadSolver m) => MonadSolver (WriterT w m) where
    send = lift . send
    recvWithTimeout = lift . recvWithTimeout
