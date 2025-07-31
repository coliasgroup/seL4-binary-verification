module BV.Search.Core.Solver
    ( MonadRepGraphSolverInteractParallel
    , MonadRepGraphSolverInteractSimple
    , RepGraphSolverInteractParallel
    , RepGraphSolverInteractParallelFailureInfo (..)
    , RepGraphSolverInteractParallelFailureReason (..)
    , RepGraphSolverInteractSimple
    , RepGraphSolverInteractSimpleFailureInfo (..)
    , RepGraphSolverInteractSimpleFailureReason (..)
    , runRepGraphSolverInteractParallel
    , runRepGraphSolverInteractSimple
    , testHyp
    , testHypWhyps
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

testHyp :: (MonadRepGraph t m, MonadRepGraphSolverInteractSimple m) => SExprWithPlaceholders -> m Bool
testHyp = checkHyp

newtype RepGraphSolverInteractSimple m a
  = RepGraphSolverInteractSimple { run :: ExceptT RepGraphSolverInteractSimpleFailureReason (ReaderT SimpleEnv m) a }
  deriving newtype (Applicative, Functor, Monad)

data SimpleEnv
  = SimpleEnv
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
    env = SimpleEnv
        { timeout
        , modelConfig
        }

--

class MonadRepGraphSolverInteractSimple m => MonadRepGraphSolverInteractParallel m where
    parallelTestHyps :: [SExprWithPlaceholders] -> m ()

instance MonadRepGraphSolverInteractParallel m => MonadRepGraphSolverInteractParallel (ReaderT r m) where
    parallelTestHyps = lift . parallelTestHyps

instance MonadRepGraphSolverInteractParallel m => MonadRepGraphSolverInteractParallel (ExceptT e m) where
    parallelTestHyps = lift . parallelTestHyps

instance MonadRepGraphSolverInteractParallel m => MonadRepGraphSolverInteractParallel (RepGraphBase t m) where
    parallelTestHyps = lift . parallelTestHyps

type RepGraphSolverInteractParallelInner m = ExceptT RepGraphSolverInteractParallelFailureReason (ReaderT (ParallelEnv m) m)

newtype RepGraphSolverInteractParallel m a
  = RepGraphSolverInteractParallel { run :: RepGraphSolverInteractParallelInner m a }
  deriving newtype (Applicative, Functor, Monad)

data ParallelEnv m
  = ParallelEnv
      { timeout :: Maybe SolverTimeout
      , modelConfig :: ModelConfig
      , runParallel :: RunParallel m
      }
  deriving (Generic)

type RunParallel m = SMTProofCheck () -> m ()

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverSend (RepGraphSolverInteractParallel m) where
    sendSExprWithPlaceholders s = RepGraphSolverInteractParallel $ do
        modelConfig <- gview #modelConfig
        sendExpectingSuccess $ configureSExpr modelConfig s

liftSimple :: RepGraphSolverInteractSimple m a -> RepGraphSolverInteractParallel m a
liftSimple m = undefined

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverInteractSimple (RepGraphSolverInteractParallel m) where
    checkHyp = liftSimple . checkHyp

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverInteractParallel (RepGraphSolverInteractParallel m) where
    parallelTestHyps hyps = undefined

testHypWhyps :: (MonadRepGraph t m, MonadRepGraphSolverInteractParallel m) => SExprWithPlaceholders -> [Hyp t] -> m Bool
testHypWhyps hyp hyps = do
    undefined
    --     do
    --         timeout <- RepGraphSolverInteractParallel $ gview #timeout
    --         modelConfig <- RepGraphSolverInteractParallel $ gview #modelConfig
    --         let sendAssert = sendSimpleCommandExpectingSuccess . Assert . Assertion . configureSExpr modelConfig
    --         let hyps = splitHyp (notS hyp)
    --         sendSimpleCommandExpectingSuccess $ Push 1
    --         traverse_ sendAssert hyps
    --         sat <- checkSatWithTimeout timeout >>= \case
    --             Nothing -> RepGraphSolverInteractParallel $ throwError RepGraphSolverInteractParallelTimedOut
    --             Just (Unknown reason) -> RepGraphSolverInteractParallel $ throwError (RepGraphSolverInteractParallelAnsweredUnknown reason)
    --             Just Sat -> return True
    --             Just Unsat -> return False
    --         sendSimpleCommandExpectingSuccess $ Pop 1
    --         unless sat $ do
    --             sendAssert $ notS (andNS hyps)
    --         return $ not sat
    --     RepGraphSolverInteractParallel $ addPValidDomAssertions
    --     undefined

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
    => RunParallel m -> Maybe SolverTimeout -> ModelConfig -> RepGraphSolverInteractParallel m a -> m (Either RepGraphSolverInteractParallelFailureReason a)
runRepGraphSolverInteractParallel runParallel timeout modelConfig m = do
    commonSolverSetup modelConfig
    runReaderT (runExceptT m.run) env
  where
    env = ParallelEnv
        { timeout
        , modelConfig
        , runParallel
        }
