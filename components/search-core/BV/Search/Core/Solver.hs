module BV.Search.Core.Solver
    ( MonadRepGraphSolverInteract
    , RepGraphSolverFailureReason (..)
    , RepGraphSolverInteractSimple
    , RepGraphSolverInteractSimpleFailureInfo (..)
    , runRepGraphSolverInteractSimple
    , testHyp
    , testHypGetModel
    , Model
    , ModelRequest
    ) where

import BV.Core.ExecuteSMTProofChecks (commonSolverSetup, splitHyp)
import BV.Core.ModelConfig
import BV.Core.RepGraph
import BV.Core.Types
import BV.Core.Types.Extras.SExprWithPlaceholders (andNS, notS)
import BV.SMTLIB2 (SExpr)
import BV.SMTLIB2.Command
import BV.SMTLIB2.Monad
import BV.Utils

import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError,
                             withExceptT)
import Control.Monad.Identity (IdentityT, runIdentityT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics

type Model = M.Map String ()

type ModelRequest = [String]

data TestResultWitModel
  = TestResultWithOptionalTrue
  | TestResultWithOptionalFalse
      { model :: Model
      }
  deriving (Generic)

data TestResultWithOptionalModel
  = TestResultWithOptionalModelTrue
  | TestResultWithOptionalModelFalse
      { model :: Maybe Model
      }
  deriving (Generic)

ensureModel :: TestResultWithOptionalModel -> TestResultWitModel
ensureModel = \case
    TestResultWithOptionalModelTrue -> TestResultWithOptionalTrue
    TestResultWithOptionalModelFalse (Just model) -> TestResultWithOptionalFalse model
    _ -> undefined

ensureNoModel :: TestResultWithOptionalModel -> Bool
ensureNoModel = \case
    TestResultWithOptionalModelTrue -> True
    TestResultWithOptionalModelFalse Nothing -> False
    _ -> undefined

class MonadRepGraphSolverSend m => MonadRepGraphSolverInteract m where
    checkHyp :: Maybe ModelRequest -> SExprWithPlaceholders -> m TestResultWithOptionalModel

instance MonadRepGraphSolverInteract m => MonadRepGraphSolverInteract (ReaderT r m) where
    checkHyp = compose2 lift checkHyp

instance MonadRepGraphSolverInteract m => MonadRepGraphSolverInteract (ExceptT e m) where
    checkHyp = compose2 lift checkHyp

instance MonadRepGraphSolverInteract m => MonadRepGraphSolverInteract (RepGraphBase t m) where
    checkHyp = compose2 lift checkHyp

testHypCommon :: Maybe ModelRequest -> SExprWithPlaceholders -> m TestResultWithOptionalModel
testHypCommon = checkHyp

testHyp :: (MonadRepGraph t m, MonadRepGraphSolverInteract m) => SExprWithPlaceholders -> m Bool
testHyp hyp = ensureNoModel <$> checkHyp Nothing hyp

testHypGetModel :: (MonadRepGraph t m, MonadRepGraphSolverInteract m) => ModelRequest -> SExprWithPlaceholders -> m TestResultWitModel
testHypGetModel modelRequest hyp = ensureModel <$> checkHyp (Just modelRequest) hyp

-- TODO fast param from graph-refine
testHypWhypsCommon
    :: (RefineTag t, MonadRepGraph t m, MonadRepGraphSolverInteract m, MonadCache c)
    => Maybe ModelRequest -> Expr -> [Hyp t] -> c m TestResultWithOptionalModel
testHypWhypsCommon modelRequestOpt hyp hyps = do
    sexpr <- lift $ interpretHypImps hyps hyp >>= withoutEnv . convertExprNoSplit
    withCache sexpr $ do
        addPValidDomAssertions
        testHypCommon modelRequestOpt sexpr

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
    checkHyp modelRequestOpt hyp = RepGraphSolverInteractSimple $ do
        timeout <- gview #timeout
        modelConfig <- gview #modelConfig
        withExceptT RepGraphSolverInteractFailureInfo $
            ExceptT $
                todo $ checkHypInner timeout modelConfig hyp

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
