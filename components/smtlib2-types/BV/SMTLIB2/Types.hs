module BV.SMTLIB2.Types
    ( module BV.SMTLIB2.Types.SExpr
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

import BV.SMTLIB2.Types.SExpr
import BV.SMTLIB2.Types.SExpr.Read

import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift)
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
