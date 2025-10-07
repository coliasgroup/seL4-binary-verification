{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Search.Core.Solver
    ( Cache
    , GraphSliceSolverFailureReason (..)
    , GraphSliceSolverInteractSimple
    , GraphSliceSolverInteractSimpleFailureInfo (..)
    , MonadGraphSliceGetSExprValue
    , MonadGraphSliceSolverInteract
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
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (Reader, ReaderT, mapReaderT, runReaderT)
import Control.Monad.State (StateT (StateT), evalStateT, mapStateT, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((.=), (<<.=))

-- TODO
-- Use an abstraction that allows for caching sat results
-- Add param to run*Solver* that determines whether ":produce-models" is sent

--

newtype Cache
  = Cache { unwrap :: M.Map FlatExpr Bool }
  deriving (Generic)

emptyCache :: Cache
emptyCache = Cache M.empty

withCache :: Monad m => StateT (Maybe Cache) m a -> StateT Cache m a
withCache (StateT f) = StateT $ \cache -> over _2 fromJust <$> f (Just cache)

withoutCache :: Monad m => StateT (Maybe Cache) m a -> m a
withoutCache (StateT f) = fst <$> f Nothing

--

class MonadGraphSliceSendSExpr m => MonadGraphSliceSolverInteract m where
    checkSExprHyp :: SExprWithPlaceholders -> m Bool

instance MonadGraphSliceSolverInteract m => MonadGraphSliceSolverInteract (ReaderT r m) where
    checkSExprHyp = lift . checkSExprHyp

instance MonadGraphSliceSolverInteract m => MonadGraphSliceSolverInteract (ExceptT e m) where
    checkSExprHyp = lift . checkSExprHyp

testHyp :: (Tag t, MonadGraphSliceSolverInteract m) => FlatExpr -> GraphSliceT t m Bool
testHyp expr = do
    sexpr <- convertExpr expr
    addAccumulatedAssertions
    lift $ checkSExprHyp sexpr

testHypWhypsCommon
    :: (RefineTag t, MonadGraphSliceSolverInteract m)
    => Bool -> FlatExpr -> [Hyp t] -> StateT (Maybe Cache) (GraphSliceT t m) (Maybe Bool)
testHypWhypsCommon fast hyp hyps = do
    expr <- lift $ interpretHypImps hyps hyp
    cacheEntry <- preuse $ #_Just % #unwrap % at expr % #_Just
    case (cacheEntry, fast) of
        (Just v, _) -> do
            return $ Just $ v
        (Nothing, True) -> do
            return Nothing
        (Nothing, False) -> do
            v <- lift $ testHyp expr
            zoomMaybe (#_Just % #unwrap) $ modify $ M.insertWith undefined expr v
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

class MonadGraphSliceSolverInteract m => MonadGraphSliceGetSExprValue m where
    getSExprValue :: SExprWithPlaceholders -> m SExpr

getFlatExprValue :: (Tag t, MonadGraphSliceGetSExprValue m) => FlatExpr -> GraphSliceT t m GraphExpr
getFlatExprValue expr = do
    sexpr <- withoutSendSExpr $ convertExpr expr
    valueSExpr <- lift $ getSExprValue sexpr
    let valueExpr = sexprToExpr valueSExpr
    ensureM $ valueExpr.ty == expr.ty
    return valueExpr

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
      { haveModel :: Bool
      }
  deriving (Generic)

data SimpleEnv
  = SimpleEnv
      { timeout :: Maybe SolverTimeout
      , modelConfig :: ModelConfig
      }
  deriving (Generic)

liftPure :: Monad m => StateT SimpleState (Reader SimpleEnv) a -> GraphSliceSolverInteractSimple m a
liftPure = GraphSliceSolverInteractSimple . lift . mapStateT (mapReaderT (return . runIdentity))

instance MonadTrans GraphSliceSolverInteractSimple where
    lift = GraphSliceSolverInteractSimple . lift . lift . lift

data GraphSliceSolverInteractSimpleFailureInfo
  = GraphSliceSolverInteractSimpleFailureInfo
      { reason :: GraphSliceSolverFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

data GraphSliceSolverFailureReason
  = GraphSliceSolverTimedOut
  | GraphSliceSolverAnsweredUnknown SExpr
  deriving (Eq, Generic, Ord, Show)

runGraphSliceSolverInteractSimple
    :: (MonadSolver m, MonadThrow m)
    => Maybe SolverTimeout -> ModelConfig -> GraphSliceSolverInteractSimple m a -> m (Either GraphSliceSolverInteractSimpleFailureInfo a)
runGraphSliceSolverInteractSimple timeout modelConfig m = do
    runReaderT (evalStateT (runExceptT m'.run) initState) env
  where
    m' = do
        commonSolverSetup
        m
    env = SimpleEnv
        { timeout
        , modelConfig
        }
    initState = SimpleState
        { haveModel = False
        }

commonSolverSetup
    :: (MonadSolver m, MonadThrow m)
    => GraphSliceSolverInteractSimple m ()
commonSolverSetup = do
    modelConfig <- liftPure $ gview #modelConfig
    lift $ do
        sendSimpleCommandExpectingSuccess $ SetOption (PrintSuccessOption True)
        sendSimpleCommandExpectingSuccess $ SetOption (ProduceModelsOption True)
        sendSimpleCommandExpectingSuccess $ SetLogic defaultLogic
        traverse_ sendExpectingSuccess (modelConfigPreamble modelConfig)

checkSatSimple
    :: (MonadSolver m, MonadThrow m)
    => GraphSliceSolverInteractSimple m Bool
checkSatSimple = do
    timeout <- liftPure $ gview #timeout
    r <- lift $ checkSatWithTimeout timeout
    case r of
        Nothing -> throwReason GraphSliceSolverTimedOut
        Just (Unknown msg) -> throwReason $ GraphSliceSolverAnsweredUnknown msg
        Just Sat -> return True
        Just Unsat -> return False
  where
    throwReason reason =
        GraphSliceSolverInteractSimple $ throwError $ GraphSliceSolverInteractSimpleFailureInfo
            { reason
            }

instance (MonadSolver m, MonadThrow m) => MonadGraphSliceSendSExpr (GraphSliceSolverInteractSimple m) where
    sendCommand s = do
        hadModel <- liftPure $ #haveModel <<.= False
        when hadModel $ do
            lift $ sendSimpleCommandExpectingSuccess $ Pop 1
        modelConfig <- liftPure $ gview #modelConfig
        lift $ sendSimpleCommandExpectingSuccess $ configureCommand modelConfig s

instance (MonadSolver m, MonadThrow m) => MonadGraphSliceSolverInteract (GraphSliceSolverInteractSimple m) where
    checkSExprHyp hyp = do
        modelConfig <- liftPure $ gview #modelConfig
        let sendAssert s =
                lift $ sendSimpleCommandExpectingSuccess $ Assert $ Assertion $ configureSExpr modelConfig s
        lift $ sendSimpleCommandExpectingSuccess $ Push 1
        traverse_ sendAssert split
        sat <- checkSatSimple
        liftPure $ #haveModel .= sat
        when (not sat) $ do
            lift $ sendSimpleCommandExpectingSuccess $ Pop 1
            sendAssert $ notS (andNS split)
        return $ not sat
      where
        split = splitHyp (notS hyp)

instance (MonadSolver m, MonadThrow m) => MonadGraphSliceGetSExprValue (GraphSliceSolverInteractSimple m) where
    getSExprValue s = do
        haveModel <- liftPure $ use #haveModel
        ensureM haveModel
        modelConfig <- liftPure $ gview #modelConfig
        r <- lift $ getValue [configureSExpr modelConfig s]
        let [value] = r
        return value
