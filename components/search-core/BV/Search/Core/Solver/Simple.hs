module BV.Search.Core.Solver.Simple
    ( MonadRepGraphSolverInteractSimple (..)
    , RepGraphSolverInteractSimple
    , RepGraphSolverInteractSimpleFailureInfo (..)
    , RepGraphSolverInteractSimpleFailureReason (..)
    , runRepGraphSolverInteractSimple
    ) where

import BV.Core.ExecuteSMTProofChecks (commonSolverSetup, splitHyp)
import BV.Core.ModelConfig
import BV.Core.RepGraph
import BV.Core.Types
import BV.Core.Types.Extras.SExprWithPlaceholders (andNS, notS)
import BV.SMTLIB2 (SExpr)
import BV.SMTLIB2.Command
import BV.SMTLIB2.Monad

import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Data.Foldable (traverse_)
import GHC.Generics (Generic)
import Optics

class MonadRepGraphSolverSend m => MonadRepGraphSolverInteractSimple m where
    checkHyp :: SExprWithPlaceholders -> m Bool

instance MonadRepGraphSolverInteractSimple m => MonadRepGraphSolverInteractSimple (ReaderT r m) where
    checkHyp = lift . checkHyp

instance MonadRepGraphSolverInteractSimple m => MonadRepGraphSolverInteractSimple (ExceptT e m) where
    checkHyp = lift . checkHyp

instance MonadRepGraphSolverInteractSimple m => MonadRepGraphSolverInteractSimple (RepGraphBase t m) where
    checkHyp = lift . checkHyp

newtype RepGraphSolverInteractSimple m a
  = RepGraphSolverInteractSimple { run :: ExceptT RepGraphSolverInteractSimpleFailureReason (ReaderT Env m) a }
  deriving (Functor)
  deriving newtype (Applicative, Monad)

data Env
  = Env
      { timeout :: Maybe SolverTimeout
      , modelConfig :: ModelConfig
      }
  deriving (Generic)

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverSend (RepGraphSolverInteractSimple m) where
    sendSExprWithPlaceholders s = RepGraphSolverInteractSimple $ do
        modelConfig <- gview #modelConfig
        sendExpectingSuccess $ configureSExpr modelConfig s

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverInteractSimple (RepGraphSolverInteractSimple m) where
    checkHyp hyp = RepGraphSolverInteractSimple $ do
        timeout <- gview #timeout
        modelConfig <- gview #modelConfig
        let sendAssert = sendSimpleCommandExpectingSuccess . Assert . Assertion . configureSExpr modelConfig
        let hyps = splitHyp (notS hyp)
        sendSimpleCommandExpectingSuccess $ Push 1
        traverse_ sendAssert hyps
        sat <- checkSatWithTimeout timeout >>= \case
            Nothing -> throwError RepGraphSolverInteractSimpleTimedOut
            Just (Unknown reason) -> throwError (RepGraphSolverInteractSimpleAnsweredUnknown reason)
            Just Sat -> return True
            Just Unsat -> return False
        sendSimpleCommandExpectingSuccess $ Pop 1
        unless sat $ do
            sendAssert $ notS (andNS hyps)
        return $ not sat

data RepGraphSolverInteractSimpleFailureInfo
  = RepGraphSolverInteractSimpleFailureInfo
      { reason :: RepGraphSolverInteractSimpleFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

data RepGraphSolverInteractSimpleFailureReason
  = RepGraphSolverInteractSimpleTimedOut
  | RepGraphSolverInteractSimpleAnsweredUnknown SExpr
  deriving (Eq, Generic, Ord, Show)

runRepGraphSolverInteractSimple
    :: (MonadSolver m, MonadThrow m)
    => Maybe SolverTimeout -> ModelConfig -> RepGraphSolverInteractSimple m a -> m (Either RepGraphSolverInteractSimpleFailureReason a)
runRepGraphSolverInteractSimple timeout modelConfig m = do
    commonSolverSetup modelConfig
    runReaderT (runExceptT m.run) env
  where
    env = Env
        { timeout
        , modelConfig
        }
