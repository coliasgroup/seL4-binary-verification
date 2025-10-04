{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Search.Core.Solver
    ( Cache
    , GraphSliceSolverFailureReason (..)
    , GraphSliceSolverInteractSimple
    , GraphSliceSolverInteractSimpleFailureInfo (..)
    , MonadGraphSliceGetSExprValue
    , MonadGraphSliceSolverInteract
    , askModelConfig
    , askTimeout
    , emptyCache
    , getFlatExprValue
    , runGraphSliceSolverInteractSimple
    , testHyp
    , testHypWhyps
    , testHypWhypsWithCache
    , testHypWhypsWithCacheFast
    , withoutSendSExpr
    ) where

import BV.Search.Core.GraphSlice

import BV.Core.ExecuteSMTProofChecks (defaultLogic, splitHyp)
import BV.Core.ModelConfig
import BV.Core.Types
import BV.Core.Types.Extras.SExprToExpr (sexprToExpr)
import BV.Core.Types.Extras.SExprWithPlaceholders (andNS, notS,
                                                   showSExprWithPlaceholders)
import BV.Logging
import BV.SMTLIB2.Command
import BV.SMTLIB2.Monad
import BV.SMTLIB2.SExpr
import BV.Utils

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError,
                             withExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT (StateT), evalStateT, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=), (.=))

-- TODO
-- Use an abstraction that allows for caching sat results

-- TODO
-- Add param to run*Solver* that determines whether ":produce-models" is sent

--

newtype Cache
  = Cache { unwrap :: M.Map SExprWithPlaceholders Bool }
  deriving (Generic)

emptyCache :: Cache
emptyCache = Cache M.empty

withCache :: Monad m => StateT (Maybe Cache) m a -> StateT Cache m a
withCache (StateT f) = StateT $ \cache -> over _2 fromJust <$> f (Just cache)

withoutCache :: Monad m => StateT (Maybe Cache) m a -> m a
withoutCache (StateT f) = fst <$> f Nothing

--

class MonadGraphSliceSendSExpr m => MonadGraphSliceSolverInteract m where
    checkHyp :: SExprWithPlaceholders -> m Bool

instance MonadGraphSliceSolverInteract m => MonadGraphSliceSolverInteract (ReaderT r m) where
    checkHyp = lift . checkHyp

instance MonadGraphSliceSolverInteract m => MonadGraphSliceSolverInteract (ExceptT e m) where
    checkHyp = lift . checkHyp

testHyp :: (Tag t, MonadGraphSliceSolverInteract m) => SExprWithPlaceholders -> GraphSliceT t m Bool
testHyp sexpr = do
    addAccumulatedAssertions
    lift $ checkHyp sexpr

testHypWhypsCommon
    :: (RefineTag t, MonadGraphSliceSolverInteract m)
    => Bool -> FlatExpr -> [Hyp t] -> StateT (Maybe Cache) (GraphSliceT t m) (Maybe Bool)
testHypWhypsCommon fast hyp hyps = do
    sexpr <- lift $ interpretHypImps hyps hyp >>= convertExpr
    cacheEntry <- preuse $ #_Just % #unwrap % at sexpr % #_Just
    case (cacheEntry, fast) of
        (Just v, _) -> do
            return $ Just $ v
        (Nothing, True) -> do
            return Nothing
        (Nothing, False) -> do
            v <- lift $ testHyp sexpr
            zoomMaybe (#_Just % #unwrap) $ modify $ M.insertWith undefined sexpr v
            return $ Just v

testHypWhyps
    :: (RefineTag t, MonadGraphSliceSolverInteract m)
    => FlatExpr -> [Hyp t] -> GraphSliceT t m Bool
testHypWhyps hyp hyps =
    fromJust <$> withoutCache (testHypWhypsCommon False hyp hyps)

testHypWhypsWithCache
    :: (RefineTag t, MonadGraphSliceSolverInteract m)
    => FlatExpr -> [Hyp t] -> StateT Cache (GraphSliceT t m) Bool
testHypWhypsWithCache hyp hyps =
    fromJust <$> withCache (testHypWhypsCommon False hyp hyps)

testHypWhypsWithCacheFast
    :: (RefineTag t, MonadGraphSliceSolverInteract m)
    => FlatExpr -> [Hyp t] -> StateT Cache (GraphSliceT t m) (Maybe Bool)
testHypWhypsWithCacheFast hyp hyps =
    withCache (testHypWhypsCommon True hyp hyps)

--

newtype DontSendSExpr a
  = DontSendSExprT { run :: Writer [SMTProofCheckCommand] a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance MonadGraphSliceSendSExpr DontSendSExpr where
    sendCommand s = DontSendSExprT $ tell [s]

withoutSendSExpr :: Monad m => GraphSliceT t DontSendSExpr a -> GraphSliceT t m a
withoutSendSExpr = mapGraphSliceT $ \m -> case runWriter m.run of
    (a, []) -> return a
    (_, ss) -> error $
        "withoutSendSExpr:\n"
            ++ concat [ showSExprWithPlaceholders (commandToSExpr s) ++ "\n" | s <- ss ]

--

class MonadGraphSliceSolverInteract m => MonadGraphSliceGetSExprValue m where
    getSExprValue :: SExprWithPlaceholders -> m SExpr

getFlatExprValue :: (Tag t, MonadGraphSliceGetSExprValue m) => FlatExpr -> GraphSliceT t m GraphExpr
getFlatExprValue expr = do
    sexpr <- withoutSendSExpr $ convertExpr expr
    valueSExpr <- lift $ getSExprValue sexpr
    let valueExpr = sexprToExpr valueSExpr
    ensureM $ valueExpr.ty == expr.ty
    return valueExpr

--

data GraphSliceSolverFailureReason
  = GraphSliceSolverTimedOut
  | GraphSliceSolverAnsweredUnknown SExpr
  deriving (Eq, Generic, Ord, Show)

checkHypInner
    :: (MonadSolver m, MonadThrow m)
    => Maybe SolverTimeout
    -> ModelConfig
    -> SExprWithPlaceholders
    -> m (Either GraphSliceSolverFailureReason Bool)
checkHypInner timeout modelConfig hyp = runExceptT $ do
    let sendAssert = sendSimpleCommandExpectingSuccess . Assert . Assertion . configureSExpr modelConfig
    let hyps = splitHyp (notS hyp)
    sendSimpleCommandExpectingSuccess $ Push 1
    traverse_ sendAssert hyps
    r <- checkSatWithTimeout timeout >>= \case
        Nothing -> throwError GraphSliceSolverTimedOut
        Just (Unknown reason) -> throwError (GraphSliceSolverAnsweredUnknown reason)
        Just Unsat -> return True
        Just Sat -> return False
    sendSimpleCommandExpectingSuccess $ Pop 1
    when r $ do
        sendAssert $ notS (andNS hyps)
    return r

commonSolverSetup
    :: (MonadSolver m, MonadThrow m)
    => ModelConfig
    -> m ()
commonSolverSetup modelConfig = do
    sendSimpleCommandExpectingSuccess $ SetOption (PrintSuccessOption True)
    sendSimpleCommandExpectingSuccess $ SetOption (ProduceModelsOption True)
    sendSimpleCommandExpectingSuccess $ SetLogic defaultLogic
    traverse_ sendExpectingSuccess (modelConfigPreamble modelConfig)

--

newtype GraphSliceSolverInteractSimple m a
  = GraphSliceSolverInteractSimple { run :: ExceptT GraphSliceSolverInteractSimpleFailureInfo (StateT SimpleState (ReaderT SimpleEnv m)) a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadLogger
    , MonadLoggerWithContext
    )

data SimpleState
  = SimpleState
      { model :: Bool
      }
  deriving (Generic)

data SimpleEnv
  = SimpleEnv
      { timeout :: Maybe SolverTimeout
      , modelConfig :: ModelConfig
      }
  deriving (Generic)

askTimeout :: Monad m => GraphSliceSolverInteractSimple m (Maybe SolverTimeout)
askTimeout = GraphSliceSolverInteractSimple $ gview #timeout

askModelConfig :: Monad m => GraphSliceSolverInteractSimple m ModelConfig
askModelConfig = GraphSliceSolverInteractSimple $ gview #modelConfig

instance MonadTrans GraphSliceSolverInteractSimple where
    lift = GraphSliceSolverInteractSimple . lift . lift . lift

instance (MonadSolver m, MonadThrow m) => MonadGraphSliceSendSExpr (GraphSliceSolverInteractSimple m) where
    sendCommand s = GraphSliceSolverInteractSimple $ do
        modelConfig <- gview #modelConfig
        sendSimpleCommandExpectingSuccess $ configureCommand modelConfig s
        #model .= False

instance (MonadSolver m, MonadThrow m) => MonadGraphSliceSolverInteract (GraphSliceSolverInteractSimple m) where
    checkHyp hyp = GraphSliceSolverInteractSimple $ do
        timeout <- gview #timeout
        modelConfig <- gview #modelConfig
        res <- withExceptT GraphSliceSolverInteractSimpleFailureInfo $
            ExceptT $ checkHypInner timeout modelConfig hyp
        when (not res) $ #model .= True
        return res

instance (MonadSolver m, MonadThrow m) => MonadGraphSliceGetSExprValue (GraphSliceSolverInteractSimple m) where
    getSExprValue s = GraphSliceSolverInteractSimple $ do
        model <- use #model
        ensureM model
        modelConfig <- gview #modelConfig
        r <- runExceptT $ getValueE [configureSExpr modelConfig s]
        let Right [value] = r
        return value

data GraphSliceSolverInteractSimpleFailureInfo
  = GraphSliceSolverInteractSimpleFailureInfo
      { reason :: GraphSliceSolverFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

runGraphSliceSolverInteractSimple
    :: (MonadSolver m, MonadThrow m)
    => Maybe SolverTimeout -> ModelConfig -> GraphSliceSolverInteractSimple m a -> m (Either GraphSliceSolverInteractSimpleFailureInfo a)
runGraphSliceSolverInteractSimple timeout modelConfig m = do
    commonSolverSetup modelConfig
    runReaderT (evalStateT (runExceptT m.run) initState) env
  where
    env = SimpleEnv
        { timeout
        , modelConfig
        }
    initState = SimpleState
        { model = False
        }

--

-- TODO

type GraphSliceSolverInteractParallelInner m =
    ExceptT GraphSliceSolverInteractParallelFailureInfo (StateT ParallelState (ReaderT (ParallelEnv m) m))

newtype GraphSliceSolverInteractParallel m a
  = GraphSliceSolverInteractParallel { run :: GraphSliceSolverInteractParallelInner m a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadLogger
    , MonadLoggerWithContext
    )

data ParallelState
  = ParallelState
      { setup :: [SMTProofCheckCommand]
      }
  deriving (Generic)

data ParallelEnv m
  = ParallelEnv
      { timeout :: Maybe SolverTimeout
      , modelConfig :: ModelConfig
      , runParallel :: RunParallel m
      }
  deriving (Generic)

type RunParallel m = m ()

instance (MonadSolver m, MonadThrow m) => MonadGraphSliceSendSExpr (GraphSliceSolverInteractParallel m) where
    sendCommand s = GraphSliceSolverInteractParallel $ do
        #setup %= (++ [s])
        modelConfig <- gview #modelConfig
        sendSimpleCommandExpectingSuccess $ configureCommand modelConfig s

instance (MonadSolver m, MonadThrow m) => MonadGraphSliceSolverInteract (GraphSliceSolverInteractParallel m) where
    checkHyp hyp = GraphSliceSolverInteractParallel $ do
        timeout <- gview #timeout
        modelConfig <- gview #modelConfig
        withExceptT GraphSliceSolverInteractParallelFailureInfo $
            ExceptT $ checkHypInner timeout modelConfig hyp

data GraphSliceSolverInteractParallelFailureInfo
  = GraphSliceSolverInteractParallelFailureInfo
      { reason :: GraphSliceSolverFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

runGraphSliceSolverInteractParallel
    :: (MonadSolver m, MonadThrow m)
    => RunParallel m -> Maybe SolverTimeout -> ModelConfig -> GraphSliceSolverInteractParallel m a -> m (Either GraphSliceSolverInteractParallelFailureInfo a)
runGraphSliceSolverInteractParallel runParallel timeout modelConfig m = do
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
        }
