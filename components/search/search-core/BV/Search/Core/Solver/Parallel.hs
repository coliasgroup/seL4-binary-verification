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
import BV.System.Utils.Stopwatch (Elapsed, elapsedToSeconds)

import BV.SMTLIB2.Process (acquireSolverContext, runSolverWithContext)
import Control.Monad (when)
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.Identity (runIdentity)
import Control.Monad.IO.Unlift (MonadUnliftIO, askUnliftIO, UnliftIO (..))
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
import BV.System.Core (OfflineSolverConfig (..))
import Text.Printf (printf)
import BV.SMTLIB2 (showSExpr)
import BV.System.Core.Utils.Logging (augmentSolverContextWithLogging)
import Control.Monad.IO.Unlift (UnliftIO(..))
import Control.Monad.IO.Unlift (UnliftIO(..))

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
      }
  deriving (Generic)

data ParallelState m
  = ParallelState
      { commands :: [SMTProofCheckCommand]
      , ctx :: Ctx
      , ctxOnline :: Bool
      , ctxHaveModel :: Bool
      }
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
    :: (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m, MonadLoggerWithContext m)
    => SolversConfig -> GraphSliceSolverInteractParallel m a -> m (Either GraphSliceSolverInteractParallelFailureInfo a)
runGraphSliceSolverInteractParallel solversConfig m = do
    ctx <- initCtx solversConfig
    let initState = ParallelState
            { commands = []
            , ctx
            , ctxOnline = True
            , ctxHaveModel = False
            }
    runReaderT (evalStateT (runExceptT m.run) initState) env
  where
    env = ParallelEnv
        { solversConfig
        }

initCtx :: forall m. (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m, MonadLoggerWithContext m) => SolversConfig -> m Ctx
initCtx solversConfig = do
    UnliftIO run <- askUnliftIO
    (releaseKey, ctx) <- allocateAcquire $
        acquireSolverContext
            (run . withPushLogContext "stderr" . logInfoGeneric)
            (solverProc online.command)
    runSolverWithContext ctx augmentSolverContextWithLogging $ do
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

instance (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m, MonadLoggerWithContext m) => MonadGraphSliceSendSExpr (GraphSliceSolverInteractParallel m) where
    sendCommand s = do
        ctx <- liftPure $ use #ctx
        ctxOnline <- liftPure $ use #ctxOnline
        ctxHaveModel <- liftPure $ use #ctxHaveModel
        if ctxOnline
            then do
                if ctxHaveModel
                    then do
                        undefined
                    else do
                        undefined
            else do
                ensureM ctxHaveModel
                undefined

            -- ParallelStateCtxOnline { ctx, haveModel } -> do
            --     liftPure $ modifying #commands (++ [s])
            --     lift $ withPushLogContext "online" $ runSolverWithContext ctx.ctx augmentSolverContextWithLogging $ sendSimpleCommandExpectingSuccess $ configureCommand ctx.modelConfig s
            -- ParallelStateCtxModel ctx -> do
            --     lift $ release ctx.releaseKey
            --     solversConfig <- liftPure $ gview #solversConfig
            --     ctx' <- lift $ initCtx solversConfig
            --     liftPure $ #ctx .= ParallelStateCtxOnline ctx'
            --     sendCommand s


popIfPushed :: (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m, MonadLoggerWithContext m) => GraphSliceSolverInteractParallel m ()
popIfPushed = do
    ctxOnline <- liftPure $ use #ctxOnline
    ensureM ctxOnline
    hadModel <- liftPure $ #ctxHaveModel <<.= False
    ctx <- liftPure $ use #ctx
    when hadModel $ lift $ sendSimpleCommandExpectingSuccess $ Pop 1


instance (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m, MonadLoggerWithContext m) => MonadGraphSliceSolverInteract (GraphSliceSolverInteractParallel m) where
    checkSExprHyp hyp = do
        undefined

instance (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m, MonadLoggerWithContext m) => MonadGraphSliceGetSExprValue (GraphSliceSolverInteractParallel m) where
    getSExprValue s = do
        undefined
        -- stateCtx <- liftPure $ use #ctx
        -- let ParallelStateCtxModel ctx = stateCtx
        -- r <- lift $ runSolverWithContext ctx.ctx augmentSolverContextWithLogging $ getValue [configureSExpr ctx.modelConfig s]
        -- let [value] = r
        -- return value

withPushLogContextOfflineSolver :: MonadLoggerWithContext m => OfflineSolverConfig -> m a -> m a
withPushLogContextOfflineSolver solver =
    withPushLogContext ("solver " ++ solver.commandName ++ " " ++ prettyModelConfig solver.modelConfig)

logOfflineSolverResult :: MonadLoggerWithContext m => Maybe SatResult -> Elapsed -> m ()
logOfflineSolverResult result elapsed = do
    case result of
        Nothing -> do
            logDebug "timeout"
        Just Sat -> do
            logDebug $ "answered sat" ++ elapsedSuffix
        Just Unsat -> do
            logDebug $ "answered unsat" ++ elapsedSuffix
        Just (Unknown reason) -> do
            logDebug $ "answered unknown: " ++ showSExpr reason ++ " " ++ elapsedSuffix
  where
    elapsedSuffix = makeElapsedSuffix elapsed

makeElapsedSuffix :: Elapsed -> String
makeElapsedSuffix elapsed = printf " (%.2fs)" (fromRational (elapsedToSeconds elapsed) :: Double)

-- TODO unify with other def
solverProc :: SolverCommand -> CreateProcess
solverProc cmd = proc cmd.path cmd.args
