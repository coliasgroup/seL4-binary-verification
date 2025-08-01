module BV.Search.Core.Solver
    ( MonadRepGraphSolverInteract
    , RepGraphSolverFailureReason (..)
    , RepGraphSolverInteractSimple
    , RepGraphSolverInteractSimpleFailureInfo (..)
    , runRepGraphSolverInteractSimple
    , testHyp
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
import Control.Monad.State (StateT, evalStateT, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (traverse_)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))
import qualified Data.Map as M
import Control.Monad.Identity (IdentityT, Identity (runIdentity), runIdentityT)

type Model = M.Map String ()

data TestResultWithOptionalModel =
        TestResultWithOptionalModelTrue
        | TestResultWithOptionalModelFalse
            { model :: Maybe Model
            }
  deriving (Generic)

data TestResultWitModel =
        TestResultWithOptionalTrue
        | TestResultWithOptionalFalse
            { model :: Model
            }
  deriving (Generic)

ensureModel :: TestResultWithOptionalModel -> TestResultWitModel
ensureModel = \case
    TestResultWithOptionalModelTrue -> TestResultWithOptionalTrue
    TestResultWithOptionalModelFalse (Just model) -> TestResultWithOptionalFalse model
    _ -> undefined

class MonadRepGraphSolverSend m => MonadRepGraphSolverInteract m where
    checkHyp :: SExprWithPlaceholders -> m Bool

instance MonadRepGraphSolverInteract m => MonadRepGraphSolverInteract (ReaderT r m) where
    checkHyp = lift . checkHyp

instance MonadRepGraphSolverInteract m => MonadRepGraphSolverInteract (ExceptT e m) where
    checkHyp = lift . checkHyp

instance MonadRepGraphSolverInteract m => MonadRepGraphSolverInteract (RepGraphBase t m) where
    checkHyp = lift . checkHyp

testHyp :: (MonadRepGraph t m, MonadRepGraphSolverInteract m) => SExprWithPlaceholders -> m Bool
testHyp = checkHyp

-- TODO fast param from graph-refine
testHypWhypsCommon
    :: (RefineTag t, MonadRepGraph t m, MonadRepGraphSolverInteract m, MonadCache c)
    => Bool -> Expr -> [Hyp t] -> c m Bool
testHypWhypsCommon model hyp hyps = do
    sexpr <- lift $ interpretHypImps hyps hyp >>= withoutEnv . convertExprNoSplit
    withCache sexpr $ do
        addPValidDomAssertions
        testHyp sexpr

testHypWhyps
    :: (RefineTag t, MonadRepGraph t m, MonadRepGraphSolverInteract m)
    => Expr -> [Hyp t] -> m Bool
testHypWhyps hyp hyps = runIdentityT $ testHypWhypsCommon False hyp hyps

testHypWhypsWithCache
    :: (RefineTag t, MonadRepGraph t m, MonadRepGraphSolverInteract m)
    => Expr -> [Hyp t] -> StateT Cache m Bool
testHypWhypsWithCache = testHypWhypsCommon False

type Cache = M.Map SExprWithPlaceholders Bool

class MonadTrans c => MonadCache c where
    withCache :: Monad m => SExprWithPlaceholders -> m Bool -> c m Bool

instance MonadCache IdentityT where
    withCache _ m = lift m

instance MonadCache (StateT Cache) where
    withCache k m = do
        opt <- use $ at k
        case opt of
            Just v -> do
                return v
            Nothing -> do
                v <- lift m
                modify $ M.insertWith undefined k v
                return v

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

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverInteract (RepGraphSolverInteractSimple m) where
    checkHyp hyp = RepGraphSolverInteractSimple $ do
        timeout <- gview #timeout
        modelConfig <- gview #modelConfig
        withExceptT RepGraphSolverInteractFailureInfo $
            ExceptT $
                checkHypInner timeout modelConfig hyp

checkHypInner :: (MonadSolver m, MonadThrow m) => Maybe SolverTimeout -> ModelConfig -> SExprWithPlaceholders -> m (Either RepGraphSolverFailureReason Bool)
checkHypInner timeout modelConfig hyp = runExceptT $ do
    let sendAssert = sendSimpleCommandExpectingSuccess . Assert . Assertion . configureSExpr modelConfig
    let hyps = splitHyp (notS hyp)
    sendSimpleCommandExpectingSuccess $ Push 1
    traverse_ sendAssert hyps
    sat <- checkSatWithTimeout timeout >>= \case
        Nothing -> throwError RepGraphSolverTimedOut
        Just (Unknown reason) -> throwError (RepGraphSolverAnsweredUnknown reason)
        Just Sat -> return True
        Just Unsat -> return False
    sendSimpleCommandExpectingSuccess $ Pop 1
    unless sat $ do
        sendAssert $ notS (andNS hyps)
    return $ not sat

data RepGraphSolverInteractSimpleFailureInfo
  = RepGraphSolverInteractFailureInfo
      { reason :: RepGraphSolverFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

data RepGraphSolverFailureReason
  = RepGraphSolverTimedOut
  | RepGraphSolverAnsweredUnknown SExpr
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
