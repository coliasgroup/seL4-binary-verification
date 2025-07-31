module BV.Search.Core.Solver.Parallel
    ( MonadRepGraphSolverInteractParallel (..)
    , RepGraphSolverInteractParallel
    , RepGraphSolverInteractParallelFailureInfo (..)
    , RepGraphSolverInteractParallelFailureReason (..)
    , runRepGraphSolverInteractParallel
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

class MonadRepGraphSolverSend m => MonadRepGraphSolverInteractParallel m where
    checkHyp :: SExprWithPlaceholders -> m Bool

instance MonadRepGraphSolverInteractParallel m => MonadRepGraphSolverInteractParallel (ReaderT r m) where
    checkHyp = lift . checkHyp

instance MonadRepGraphSolverInteractParallel m => MonadRepGraphSolverInteractParallel (ExceptT e m) where
    checkHyp = lift . checkHyp

instance MonadRepGraphSolverInteractParallel m => MonadRepGraphSolverInteractParallel (RepGraphBase t m) where
    checkHyp = lift . checkHyp

type RepGraphSolverInteractParallelInner m = ExceptT RepGraphSolverInteractParallelFailureReason (ReaderT Env m)

newtype RepGraphSolverInteractParallel m a
  = RepGraphSolverInteractParallel { run :: RepGraphSolverInteractParallelInner m a }
  deriving newtype (Applicative, Functor, Monad)

data Env
  = Env
      { timeout :: Maybe SolverTimeout
      , modelConfig :: ModelConfig
      }
  deriving (Generic)

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverSend (RepGraphSolverInteractParallel m) where
    sendSExprWithPlaceholders s = RepGraphSolverInteractParallel $ do
        modelConfig <- gview #modelConfig
        sendExpectingSuccess $ configureSExpr modelConfig s

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverInteractParallel (RepGraphSolverInteractParallel m) where
    checkHyp hyp = RepGraphSolverInteractParallel $ do
        timeout <- gview #timeout
        modelConfig <- gview #modelConfig
        let sendAssert = sendSimpleCommandExpectingSuccess . Assert . Assertion . configureSExpr modelConfig
        let hyps = splitHyp (notS hyp)
        sendSimpleCommandExpectingSuccess $ Push 1
        traverse_ sendAssert hyps
        sat <- checkSatWithTimeout timeout >>= \case
            Nothing -> throwError RepGraphSolverInteractParallelTimedOut
            Just (Unknown reason) -> throwError (RepGraphSolverInteractParallelAnsweredUnknown reason)
            Just Sat -> return True
            Just Unsat -> return False
        sendSimpleCommandExpectingSuccess $ Pop 1
        unless sat $ do
            sendAssert $ notS (andNS hyps)
        return $ not sat

data RepGraphSolverInteractParallelFailureInfo
  = RepGraphSolverInteractParallelFailureInfo
      { reason :: RepGraphSolverInteractParallelFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

data RepGraphSolverInteractParallelFailureReason
  = RepGraphSolverInteractParallelTimedOut
  | RepGraphSolverInteractParallelAnsweredUnknown SExpr
  deriving (Eq, Generic, Ord, Show)

runRepGraphSolverInteractParallel
    :: (MonadSolver m, MonadThrow m)
    => Maybe SolverTimeout -> ModelConfig -> RepGraphSolverInteractParallel m a -> m (Either RepGraphSolverInteractParallelFailureReason a)
runRepGraphSolverInteractParallel timeout modelConfig m = do
    commonSolverSetup modelConfig
    runReaderT (runExceptT m.run) env
  where
    env = Env
        { timeout
        , modelConfig
        }
