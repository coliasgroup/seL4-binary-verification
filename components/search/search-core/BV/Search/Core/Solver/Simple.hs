{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Search.Core.Solver.Simple
    ( GraphSliceSolverFailureReason (..)
    , GraphSliceSolverInteractSimple
    , GraphSliceSolverInteractSimpleFailureInfo (..)
    , runGraphSliceSolverInteractSimple
    ) where

import BV.Search.Core.Solver.Common

import BV.Core.ExecuteSMTProofChecks (defaultLogic, splitHyp)
import BV.Core.ModelConfig
import BV.Core.Types
import BV.Core.Types.Extras.SExprWithPlaceholders (andNS, notS)
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
import Control.Monad.State (StateT (StateT), evalStateT, mapStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (traverse_)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((.=), (<<.=))

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
