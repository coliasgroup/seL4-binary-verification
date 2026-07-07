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
import BV.SMTLIB2.Process (SolverContext, runSolverT)
import BV.SMTLIB2.SExpr
import BV.System.Core (OnlineSolverConfig (..), SolverCommand (..),
                       SolversConfig (..))
import BV.Utils

import BV.SMTLIB2.Process (acquireSolverContext, runSolverWithContext)
import Control.Monad (when)
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.Identity (runIdentity)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (Reader, ReaderT, mapReaderT, runReaderT)
import Control.Monad.State (StateT (StateT), evalStateT, mapStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, release)
import Data.Acquire (allocateAcquire)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((.=), (<<.=))
import System.Process (CreateProcess, proc)

type StderrSink = T.Text -> IO ()

newtype GraphSliceSolverInteractParallel m a
  = GraphSliceSolverInteractParallel { run :: ExceptT GraphSliceSolverInteractParallelFailureInfo (StateT (ParallelState m) (ReaderT (ParallelEnv m) m)) a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadLogger
    , MonadLoggerWithContext
    )

data ParallelEnv m
  = ParallelEnv
      { solversConfig :: SolversConfig
      , modifyCtx :: SolverContext m -> SolverContext m
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
  = ParallelStateCtxOnline Ctx
  | ParallelStateCtxModel Ctx
  deriving (Generic)

data Ctx
  = Ctx
      { ctx :: SolverContext IO
      , modelConfig :: ModelConfig
      , releaseKey :: ReleaseKey
      }
  deriving (Generic)

liftPure :: Monad m => StateT (ParallelState m) (Reader ((ParallelEnv m))) a -> GraphSliceSolverInteractParallel m a
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
    :: (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m)
    => SolversConfig -> (SolverContext m -> SolverContext m) -> StderrSink -> GraphSliceSolverInteractParallel m a -> m (Either GraphSliceSolverInteractParallelFailureInfo a)
runGraphSliceSolverInteractParallel solversConfig modifyCtx stderrSink m = do
    let initState = ParallelState
            { commands = []
            , ctx = undefined
            }
    runReaderT (evalStateT (runExceptT m.run) initState) env
  where
    env = ParallelEnv
        { solversConfig
        , modifyCtx
        , stderrSink
        }

initCtx :: (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m) => (SolverContext m -> SolverContext m) -> StderrSink -> SolversConfig -> m Ctx
initCtx modifyCtx stderrSink solversConfig = do
    (releaseKey, ctx) <- allocateAcquire $ acquireSolverContext stderrSink (solverProc online.command)
    runSolverWithContext ctx modifyCtx $ do
        sendSimpleCommandExpectingSuccess $ SetOption (PrintSuccessOption True)
        sendSimpleCommandExpectingSuccess $ SetOption (ProduceModelsOption True)
        sendSimpleCommandExpectingSuccess $ SetLogic defaultLogic
        traverse_ sendExpectingSuccess (modelConfigPreamble online.modelConfig)
    return $ Ctx
      { ctx
      , modelConfig = online.modelConfig
      , releaseKey = releaseKey
      }
  where
    Just online = solversConfig.online

instance (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m) => MonadGraphSliceSendSExpr (GraphSliceSolverInteractParallel m) where
    sendCommand s = do
        stateCtx <- liftPure $ use #ctx
        case stateCtx of
            ParallelStateCtxOnline ctx -> do
                liftPure $ modifying #commands (++ [s])
                modifyCtx <- liftPure $ gview #modifyCtx
                lift $ runSolverWithContext ctx.ctx modifyCtx $ sendSimpleCommandExpectingSuccess $ configureCommand ctx.modelConfig s
            ParallelStateCtxModel ctx -> do
                lift $ release ctx.releaseKey
                modifyCtx <- liftPure $ gview #modifyCtx
                solversConfig <- liftPure $ gview #solversConfig
                stderrSink <- liftPure $ gview #stderrSink
                ctx' <- lift $ initCtx modifyCtx stderrSink solversConfig
                liftPure $ #ctx .= ParallelStateCtxOnline ctx'
                sendCommand s

instance (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m) => MonadGraphSliceSolverInteract (GraphSliceSolverInteractParallel m) where
    checkSExprHyp hyp = do
        undefined

instance (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m) => MonadGraphSliceGetSExprValue (GraphSliceSolverInteractParallel m) where
    getSExprValue s = do
        stateCtx <- liftPure $ use #ctx
        let ParallelStateCtxModel ctx = stateCtx
        modifyCtx <- liftPure $ gview #modifyCtx
        r <- lift $ runSolverWithContext ctx.ctx modifyCtx $ getValue [configureSExpr ctx.modelConfig s]
        let [value] = r
        return value

-- TODO unify with other def
solverProc :: SolverCommand -> CreateProcess
solverProc cmd = proc cmd.path cmd.args
