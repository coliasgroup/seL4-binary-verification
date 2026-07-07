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
import BV.SMTLIB2.Process (SolverContext, runSolverT, SolverT)
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
import Data.Maybe (fromJust)

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
      }
  deriving (Generic)

data Ctx
  = Ctx
      { ctx :: SolverContext IO
      , releaseKey :: ReleaseKey
      , config :: CtxSolverConfig
      , haveModel :: Bool
      }
  deriving (Generic)

data CtxSolverConfig
  = CtxSolverConfigOnline OnlineSolverConfig
  | CtxSolverConfigOffline OfflineSolverConfig
  deriving (Eq, Generic, Ord, Show)

modelConfigOf :: CtxSolverConfig -> ModelConfig
modelConfigOf = \case
    CtxSolverConfigOnline config -> config.modelConfig
    CtxSolverConfigOffline config -> config.modelConfig

procOf :: CtxSolverConfig -> CreateProcess
procOf = solverProc . \case
    CtxSolverConfigOnline config -> config.command
    CtxSolverConfigOffline config -> config.command

withPushSolverConfigLogContext :: MonadLoggerWithContext m => CtxSolverConfig -> m a -> m a
withPushSolverConfigLogContext = \case
    CtxSolverConfigOnline _ ->
        withPushLogContext "online"
    CtxSolverConfigOffline config ->
        withPushLogContext "offline" .
            withPushLogContext ("solver " ++ config.commandName ++ " " ++ prettyModelConfig config.modelConfig)

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
    ctx <- initCtx (CtxSolverConfigOnline (fromJust solversConfig.online))
    let initState = ParallelState
            { commands = []
            , ctx
            }
    runReaderT (evalStateT (runExceptT m.run) initState) env
  where
    env = ParallelEnv
        { solversConfig
        }

initCtx
    :: forall m.
       ( MonadUnliftIO m
       , MonadThrow m
       , MonadMask m
       , MonadResource m
       , MonadLoggerWithContext m
       )
    => CtxSolverConfig
    -> m Ctx
initCtx config = do
    UnliftIO run <- askUnliftIO
    (releaseKey, ctx) <- allocateAcquire $
        acquireSolverContext
            (run . withPushSolverConfigLogContext config . withPushLogContext "stderr" . logInfoGeneric)
            (procOf config)
    let this = Ctx
            { ctx
            , releaseKey = releaseKey
            , config
            , haveModel = False
            }
    useCtx this $ \modelConfig -> do
        sendSimpleCommandExpectingSuccess $ SetOption (PrintSuccessOption True)
        sendSimpleCommandExpectingSuccess $ SetOption (ProduceModelsOption True)
        sendSimpleCommandExpectingSuccess $ SetLogic defaultLogic
        traverse_ sendExpectingSuccess (modelConfigPreamble modelConfig)
    return this

useCtx
   :: ( MonadUnliftIO m
      , MonadThrow m
      , MonadMask m
      , MonadResource m
      , MonadLoggerWithContext m
      )
    => Ctx
    -> (ModelConfig -> SolverT m a)
    -> m a
useCtx ctx m = do
    withPushSolverConfigLogContext ctx.config $
        runSolverWithContext ctx.ctx augmentSolverContextWithLogging $
            m (modelConfigOf ctx.config)

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
