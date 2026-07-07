{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Search.Core.Solver.Parallel
    ( GraphSliceSolverFailureReason (..)
    , GraphSliceSolverInteractParallel
    , GraphSliceSolverInteractParallelFailureInfo (..)
    , runGraphSliceSolverInteractParallel
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
import BV.System.Core (SolversConfig)
import BV.Utils
import BV.SMTLIB2.Process (SolverContext, runSolverT)

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
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (ReleaseKey, release, MonadResource)
import qualified Data.Text as T
import Data.Acquire (allocateAcquire)

type StderrSink = T.Text -> IO ()

newtype GraphSliceSolverInteractParallel m a
  = GraphSliceSolverInteractParallel { run :: ExceptT GraphSliceSolverInteractParallelFailureInfo (StateT (ParallelState m) (ReaderT ParallelEnv m)) a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadLogger
    , MonadLoggerWithContext
    )

data ParallelEnv
  = ParallelEnv
      { solversConfig :: SolversConfig
      , stderrSink :: StderrSink
      }
  deriving (Generic)

data ParallelState m
  = ParallelState
      { commands :: [SMTProofCheckCommand]
      , ctx :: ParallelStateCtx m
      }
  deriving (Generic)

data ParallelStateCtx m
  = ParallelStateCtxOnline (Ctx m)
  | ParallelStateCtxModel (Ctx m)
  deriving (Generic)

data Ctx m
  = Ctx
      { ctx :: SolverContext m
      , modelConfig :: ModelConfig
      , releaseKey :: ReleaseKey
      }
  deriving (Generic)

liftPure :: Monad m => StateT (ParallelState m) (Reader ParallelEnv) a -> GraphSliceSolverInteractParallel m a
liftPure = GraphSliceSolverInteractParallel . lift . mapStateT (mapReaderT (return . runIdentity))

instance MonadTrans GraphSliceSolverInteractParallel where
    lift = GraphSliceSolverInteractParallel . lift . lift . lift

data GraphSliceSolverInteractParallelFailureInfo
  = GraphSliceSolverInteractParallelFailureInfo
      { reason :: GraphSliceSolverFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

data GraphSliceSolverFailureReason
  = GraphSliceSolverTimedOut
  | GraphSliceSolverAnsweredUnknown SExpr
  deriving (Eq, Generic, Ord, Show)

runGraphSliceSolverInteractParallel
    :: (MonadUnliftIO m, MonadThrow m, MonadResource m)
    => SolversConfig -> StderrSink -> GraphSliceSolverInteractParallel m a -> m (Either GraphSliceSolverInteractParallelFailureInfo a)
runGraphSliceSolverInteractParallel solversConfig stderrSink m = do
    runReaderT (evalStateT (runExceptT m'.run) initState) env
  where
    m' = do
        -- commonSolverSetup
        m
    env = ParallelEnv
        { solversConfig
        , stderrSink
        }
    initState = ParallelState
        { commands = []
        , ctx = undefined
        }

initCtx :: (MonadUnliftIO m, MonadThrow m, MonadResource m) => StderrSink -> SolversConfig -> m (Ctx m)
initCtx solversConfig = do
    allocateAcquire $ acquireSolverContext
    undefined
    -- lift $ do
    --     sendSimpleCommandExpectingSuccess $ SetOption (PrintSuccessOption True)
    --     sendSimpleCommandExpectingSuccess $ SetOption (ProduceModelsOption True)
    --     sendSimpleCommandExpectingSuccess $ SetLogic defaultLogic
    --     traverse_ sendExpectingSuccess (modelConfigPreamble modelConfig)
  where
    Just online = solversConfig.online

instance (MonadUnliftIO m, MonadThrow m, MonadResource m) => MonadGraphSliceSendSExpr (GraphSliceSolverInteractParallel m) where
    sendCommand s = do
        stateCtx <- liftPure $ use #ctx
        case stateCtx of
            ParallelStateCtxOnline ctx -> do
                liftPure $ modifying #commands (++ [s])
                lift $ flip runSolverT ctx.ctx $ sendSimpleCommandExpectingSuccess $ configureCommand ctx.modelConfig s
            ParallelStateCtxModel ctx -> do
                lift $ release ctx.releaseKey
                solversConfig <- liftPure $ gview #solversConfig
                stderrSink <- liftPure $ gview #stderrSink
                ctx' <- lift $ initCtx stderrSink solversConfig
                liftPure $ #ctx .= ParallelStateCtxOnline ctx'
                sendCommand s

instance (MonadUnliftIO m, MonadThrow m, MonadResource m) => MonadGraphSliceSolverInteract (GraphSliceSolverInteractParallel m) where
    checkSExprHyp hyp = do
        undefined

instance (MonadUnliftIO m, MonadThrow m, MonadResource m) => MonadGraphSliceGetSExprValue (GraphSliceSolverInteractParallel m) where
    getSExprValue s = do
        stateCtx <- liftPure $ use #ctx
        let ParallelStateCtxModel ctx = stateCtx
        r <- lift $ flip runSolverT ctx.ctx $ getValue [configureSExpr ctx.modelConfig s]
        let [value] = r
        return value
