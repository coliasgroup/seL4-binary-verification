module BV.Search.Solver
    ( MonadRepGraphSolverInteract (..)
    , SimpleSolver
    , SimpleSolverFailureInfo (..)
    , SimpleSolverFailureReason (..)
    , runSimpleSolver
    ) where

import BV.Core.ExecuteSMTProofChecks (commonSolverSetup, splitHyp)
import BV.Core.ModelConfig
import BV.Core.RepGraph
import BV.Core.Types
import BV.Core.Types.Extras.SExprWithPlaceholders (andNS, notS)
import BV.SMTLIB2.Command
import BV.SMTLIB2.Monad

import BV.SMTLIB2 (SExpr)
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Data.Foldable (traverse_)
import GHC.Generics (Generic)
import Optics

class MonadRepGraphSolverSend m => MonadRepGraphSolverInteract m where
    checkHyp :: SExprWithPlaceholders -> m Bool

instance MonadRepGraphSolverInteract m => MonadRepGraphSolverInteract (ReaderT r m) where
    checkHyp = lift . checkHyp

instance MonadRepGraphSolverInteract m => MonadRepGraphSolverInteract (ExceptT e m) where
    checkHyp = lift . checkHyp

instance MonadRepGraphSolverInteract m => MonadRepGraphSolverInteract (RepGraphBase t m) where
    checkHyp = lift . checkHyp

newtype SimpleSolver m a
  = SimpleSolver { run :: ExceptT SimpleSolverFailureReason (ReaderT Env m) a }
  deriving (Functor)
  deriving newtype (Applicative, Monad)

data Env
  = Env
      { timeout :: Maybe SolverTimeout
      , modelConfig :: ModelConfig
      }
  deriving (Generic)

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverSend (SimpleSolver m) where
    sendSExprWithPlaceholders s = SimpleSolver $ do
        modelConfig <- gview #modelConfig
        sendExpectingSuccess $ configureSExpr modelConfig s

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverInteract (SimpleSolver m) where
    checkHyp hyp = SimpleSolver $ do
        timeout <- gview #timeout
        modelConfig <- gview #modelConfig
        let sendAssert = sendSimpleCommandExpectingSuccess . Assert . Assertion . configureSExpr modelConfig
        let hyps = splitHyp (notS hyp)
        sendSimpleCommandExpectingSuccess $ Push 1
        traverse_ sendAssert hyps
        sat <- checkSatWithTimeout timeout >>= \case
            Nothing -> throwError SimpleSolverTimedOut
            Just (Unknown reason) -> throwError (SimpleSolverAnsweredUnknown reason)
            Just Sat -> return True
            Just Unsat -> return False
        sendSimpleCommandExpectingSuccess $ Pop 1
        unless sat $ do
            sendAssert $ notS (andNS hyps)
        return $ not sat

data SimpleSolverFailureInfo
  = SimpleSolverFailureInfo
      { reason :: SimpleSolverFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

data SimpleSolverFailureReason
  = SimpleSolverTimedOut
  | SimpleSolverAnsweredUnknown SExpr
  deriving (Eq, Generic, Ord, Show)

runSimpleSolver
    :: (MonadSolver m, MonadThrow m)
    => Maybe SolverTimeout -> ModelConfig -> SimpleSolver m a -> m (Either SimpleSolverFailureReason a)
runSimpleSolver timeout modelConfig m = do
    commonSolverSetup modelConfig
    runReaderT (runExceptT m.run) env
  where
    env = Env
        { timeout
        , modelConfig
        }
