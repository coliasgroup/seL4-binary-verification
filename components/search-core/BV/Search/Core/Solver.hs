module BV.Search.Core.Solver
    ( MonadRepGraphSolverInteractParallel
    , MonadRepGraphSolverInteractSimple
    , RepGraphOnlineSolverFailureReason (..)
    , RepGraphSolverInteractParallel
    , RepGraphSolverInteractParallelFailureInfo (..)
    , RepGraphSolverInteractParallelFailureReason (..)
    , RepGraphSolverInteractSimple
    , RepGraphSolverInteractSimpleFailureInfo (..)
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
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError,
                             withExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (traverse_)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))

-- type Model = M.Map String ()

-- data TestResultWithOptionalModel =
--         TestResultWithOptionalModelTrue
--         | TestResultWithOptionalModelFalse
--             { model :: Maybe Model
--             }
--   deriving (Generic)

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
  = RepGraphSolverInteractSimple { run :: ExceptT RepGraphSolverInteractSimpleFailureInfo (ReaderT SimpleEnv m) a }
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
        withExceptT RepGraphSolverInteractSimpleFailureInfo $
            ExceptT $
                checkHypInner timeout modelConfig hyp

checkHypInner :: (MonadSolver m, MonadThrow m) => Maybe SolverTimeout -> ModelConfig -> SExprWithPlaceholders -> m (Either RepGraphOnlineSolverFailureReason Bool)
checkHypInner timeout modelConfig hyp = runExceptT $ do
    let sendAssert = sendSimpleCommandExpectingSuccess . Assert . Assertion . configureSExpr modelConfig
    let hyps = splitHyp (notS hyp)
    sendSimpleCommandExpectingSuccess $ Push 1
    traverse_ sendAssert hyps
    sat <- checkSatWithTimeout timeout >>= \case
        Nothing -> throwError RepGraphOnlineSolverTimedOut
        Just (Unknown reason) -> throwError (RepGraphOnlineSolverAnsweredUnknown reason)
        Just Sat -> return True
        Just Unsat -> return False
    sendSimpleCommandExpectingSuccess $ Pop 1
    unless sat $ do
        sendAssert $ notS (andNS hyps)
    return $ not sat

data RepGraphSolverInteractSimpleFailureInfo
  = RepGraphSolverInteractSimpleFailureInfo
      { reason :: RepGraphOnlineSolverFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

data RepGraphOnlineSolverFailureReason
  = RepGraphOnlineSolverTimedOut
  | RepGraphOnlineSolverAnsweredUnknown SExpr
  deriving (Eq, Generic, Ord, Show)

runRepGraphSolverInteractSimple
    :: (MonadSolver m, MonadThrow m)
    => Maybe SolverTimeout -> ModelConfig -> RepGraphSolverInteractSimple m a -> m (Either RepGraphSolverInteractSimpleFailureInfo a)
runRepGraphSolverInteractSimple timeout modelConfig m = do
    commonSolverSetup modelConfig
    runReaderT (runExceptT m.run) env
  where
    env = SimpleEnv
        { timeout
        , modelConfig
        }

--

class MonadRepGraphSolverSend m => MonadRepGraphSolverInteractParallel m where
    parallelCheckHyp :: SExprWithPlaceholders -> m Bool

instance MonadRepGraphSolverInteractParallel m => MonadRepGraphSolverInteractParallel (ReaderT r m) where
    parallelCheckHyp = lift . parallelCheckHyp

instance MonadRepGraphSolverInteractParallel m => MonadRepGraphSolverInteractParallel (ExceptT e m) where
    parallelCheckHyp = lift . parallelCheckHyp

instance MonadRepGraphSolverInteractParallel m => MonadRepGraphSolverInteractParallel (RepGraphBase t m) where
    parallelCheckHyp = lift . parallelCheckHyp

type RepGraphSolverInteractParallelInner m =
    ExceptT RepGraphSolverInteractParallelFailureInfo (StateT ParallelState (ReaderT (ParallelEnv m) m))

newtype RepGraphSolverInteractParallel m a
  = RepGraphSolverInteractParallel { run :: RepGraphSolverInteractParallelInner m a }
  deriving newtype (Applicative, Functor, Monad)

instance MonadTrans (RepGraphSolverInteractParallel) where
    lift = RepGraphSolverInteractParallel . lift . lift . lift

data ParallelEnv m
  = ParallelEnv
      { timeout :: Maybe SolverTimeout
      , modelConfig :: ModelConfig
      , runParallel :: RunParallel (RepGraphSolverInteractParallel m)
      }
  deriving (Generic)

data ParallelState
  = ParallelState
      { setup :: [SExprWithPlaceholders]
      , isOnlineSolverClosed :: Bool
      }
  deriving (Generic)

type RunParallel m = SMTProofCheck () -> m Bool

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverSend (RepGraphSolverInteractParallel m) where
    sendSExprWithPlaceholders s = RepGraphSolverInteractParallel $ do
        #setup %= (++ [s])
        modelConfig <- gview #modelConfig
        sendExpectingSuccess $ configureSExpr modelConfig s

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverInteractSimple (RepGraphSolverInteractParallel m) where
    checkHyp hyp = RepGraphSolverInteractParallel $ do
        timeout <- gview #timeout
        modelConfig <- gview #modelConfig
        withExceptT (RepGraphSolverInteractParallelFailureInfo . RepGraphSolverInteractParallelFailureReasonOnlineSolverFailed) $
            ExceptT $
                checkHypInner timeout modelConfig hyp

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverInteractParallel (RepGraphSolverInteractParallel m) where
    parallelCheckHyp hyp = do
        timeout <- RepGraphSolverInteractParallel $ gview #timeout
        modelConfig <- RepGraphSolverInteractParallel $ gview #modelConfig
        r <- lift $ checkHypInner timeout modelConfig hyp
        case r of
            Right res -> return res
            Left _ -> do
                setup <- RepGraphSolverInteractParallel $ use #setup
                let check = SMTProofCheck
                        { setup
                        , imp = SMTProofCheckImp
                            { meta = ()
                            , term = hyp
                            }
                        }
                runParallel <- RepGraphSolverInteractParallel $ gview #runParallel
                runParallel check

testHypWhyps :: (RefineTag t, MonadRepGraph t m, MonadRepGraphSolverInteractParallel m) => Expr -> [Hyp t] -> m Bool
testHypWhyps hyp hyps = do
    expr <- interpretHypImps hyps hyp
    sexpr <- withoutEnv $ convertExprNoSplit expr
    -- check cache
    -- fail if fast
    addPValidDomAssertions
    parallelCheckHyp sexpr

data RepGraphSolverInteractParallelFailureInfo
  = RepGraphSolverInteractParallelFailureInfo
      { reason :: RepGraphSolverInteractParallelFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

data RepGraphSolverInteractParallelFailureReason
  = RepGraphSolverInteractParallelFailureReasonOnlineSolverFailed RepGraphOnlineSolverFailureReason
  deriving (Eq, Generic, Ord, Show)

runRepGraphSolverInteractParallel
    :: (MonadSolver m, MonadThrow m)
    => RunParallel (RepGraphSolverInteractParallel m)
    -> Maybe SolverTimeout
    -> ModelConfig
    -> RepGraphSolverInteractParallel m a
    -> m (Either RepGraphSolverInteractParallelFailureInfo a)
runRepGraphSolverInteractParallel runParallel timeout modelConfig m = do
    commonSolverSetup modelConfig
    runReaderT (evalStateT (runExceptT m.run) initState) env
  where
    env = ParallelEnv
        { timeout
        , modelConfig
        , runParallel
        }
    initState = ParallelState
        { setup = []
        , isOnlineSolverClosed = False
        }
