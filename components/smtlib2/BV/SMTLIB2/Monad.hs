module BV.SMTLIB2.Monad
    ( MonadSolver (..)
    , SolverTimeout
    , recvSExpr
    , solverTimeoutFromSeconds
    , solverTimeoutToSeconds
    ) where

import BV.SMTLIB2.SExpr

import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT)
import Data.Binary (Binary)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

newtype SolverTimeout
  = SolverTimeout { seconds :: Integer }
  deriving (Binary, Eq, Generic, Ord, Show)

solverTimeoutFromSeconds :: HasCallStack => Integer -> SolverTimeout
solverTimeoutFromSeconds seconds =
    if seconds < 0
    then error "negative timeout"
    else SolverTimeout seconds

solverTimeoutToSeconds :: SolverTimeout -> Integer
solverTimeoutToSeconds = (.seconds)

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
