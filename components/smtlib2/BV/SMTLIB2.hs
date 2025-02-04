module BV.SMTLIB2
    ( module BV.SMTLIB2.SExpr
    , module BV.SMTLIB2.SExpr.Show
    , MonadSolver (..)
    , SolverTimeout (..)
    , readSExpr
    , readSExprs
    , recvSExpr
    , tryReadSExpr
    , tryReadSExprs
    ) where

import BV.SMTLIB2.SExpr
import BV.SMTLIB2.SExpr.Parse.Megaparsec (readSExpr, readSExprs, tryReadSExpr,
                                          tryReadSExprs)
import BV.SMTLIB2.SExpr.Show

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
    sendSExpr :: SExpr -> m ()
    recvSExprWithTimeout :: Maybe SolverTimeout -> m (Maybe SExpr)

recvSExpr :: MonadSolver m => m SExpr
recvSExpr = fromJust <$> recvSExprWithTimeout Nothing

instance MonadSolver m => MonadSolver (ExceptT e m) where
    sendSExpr = lift . sendSExpr
    recvSExprWithTimeout = lift . recvSExprWithTimeout

instance MonadSolver m => MonadSolver (StateT s m) where
    sendSExpr = lift . sendSExpr
    recvSExprWithTimeout = lift . recvSExprWithTimeout

instance (Monoid w, MonadSolver m) => MonadSolver (WriterT w m) where
    sendSExpr = lift . sendSExpr
    recvSExprWithTimeout = lift . recvSExprWithTimeout
