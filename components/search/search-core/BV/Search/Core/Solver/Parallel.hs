{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Search.Core.Solver.Parallel
    ( GraphSliceSolverInteractParallel
    , GraphSliceSolverInteractParallelFailureInfo (..)
    , GraphSliceSolverInteractParallelFailureReason (..)
    , runGraphSliceSolverInteractParallel
    ) where

import BV.Search.Core.Solver.Common

import BV.Core.ExecuteSMTProofChecks (defaultLogic, splitHyp)
import BV.Core.ModelConfig
import BV.Core.Types
import BV.Core.Types.Extras.SExprWithPlaceholders (andNS, notS)
import BV.Logging
import BV.SMTLIB2 (showSExpr)
import BV.SMTLIB2.Command
import BV.SMTLIB2.Process (SolverContext, SolverT, acquireSolverContext,
                           runSolverWithContext)
import BV.SMTLIB2.SExpr
import BV.System.Core (OfflineSolverConfig (..), OnlineSolverConfig (..),
                       SolverCommand (..), SolversConfig (..),
                       offlineSolverConfigsForSingleCheck)
import BV.System.Core.Utils.Logging (augmentSolverContextWithLogging)
import BV.System.Utils.Stopwatch (Elapsed, elapsedToSeconds)
import BV.System.Utils.UnliftIO.Async (forConcurrentlyUnliftIOE)
import BV.Utils

import Control.Monad (when)
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.Identity (runIdentity)
import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (..), askUnliftIO)
import Control.Monad.Reader (Reader, ReaderT, mapReaderT, runReaderT)
import Control.Monad.State (StateT (StateT), evalStateT, mapStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Resource (MonadResource (liftResourceT), ReleaseKey,
                                     release)
import Data.Acquire (allocateAcquire)
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=), (.=), (<<.=))
import System.Process (CreateProcess, proc)
import Text.Printf (printf)

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

isOnline :: Ctx -> Bool
isOnline ctx = is #_CtxSolverConfigOnline ctx.config

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
      { reason :: GraphSliceSolverInteractParallelFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

data GraphSliceSolverInteractParallelFailureReason
  = GraphSliceSolverTimedOut
  | GraphSliceSolverAnsweredUnknown CtxSolverConfig SExpr
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

onboardCtx
    :: forall m.
       ( MonadUnliftIO m
       , MonadThrow m
       , MonadMask m
       , MonadResource m
       , MonadLoggerWithContext m
       )
    => [SMTProofCheckCommand]
    -> CtxSolverConfig
    -> m Ctx
onboardCtx commands config = do
    this <- initCtx config
    useCtx this $ \modelConfig -> do
        for_ commands $ \s -> do
            sendSimpleCommandExpectingSuccess $ configureCommand modelConfig s
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

useCtxM
   :: ( MonadUnliftIO m
      , MonadThrow m
      , MonadMask m
      , MonadResource m
      , MonadLoggerWithContext m
      )
    => (ModelConfig -> SolverT m a)
    -> GraphSliceSolverInteractParallel m a
useCtxM m = do
    ctx <- liftPure $ use #ctx
    lift $ useCtx ctx m

returnToOnline
    :: ( MonadUnliftIO m
       , MonadThrow m
       , MonadMask m
       , MonadResource m
       , MonadLoggerWithContext m
       )
    => GraphSliceSolverInteractParallel m ()
returnToOnline = do
    online <- liftPure $ use $ #ctx % to isOnline
    if online
        then do
            hadModel <- liftPure $ #ctx % #haveModel <<.= False
            when hadModel $ useCtxM $ \_ -> sendSimpleCommandExpectingSuccess $ Pop 1
        else do
            commands <- liftPure $ use #commands
            onlineConfig <- liftPure $ gview $ #solversConfig % #online % unwrapped
            ctx <- lift $ onboardCtx commands $ CtxSolverConfigOnline onlineConfig
            liftPure $ #ctx .= ctx

instance (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m, MonadLoggerWithContext m) => MonadGraphSliceSendSExpr (GraphSliceSolverInteractParallel m) where
    sendCommand s = do
        returnToOnline
        liftPure $ #commands %= (++ [s])
        useCtxM $ \modelConfig ->
            sendSimpleCommandExpectingSuccess $ configureCommand modelConfig s

instance (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m, MonadLoggerWithContext m) => MonadGraphSliceSolverInteract (GraphSliceSolverInteractParallel m) where
    checkSExprHyp hyp = do
        returnToOnline
        config <- liftPure $ use $ #ctx % #config % expecting #_CtxSolverConfigOnline
        let timeout = config.timeout
        r <- useCtxM $ \modelConfig -> do
            sendSimpleCommandExpectingSuccess $ Push 1
            for_ split $ \s -> do
                sendSimpleCommandExpectingSuccess $ Assert $ Assertion $ configureSExpr modelConfig s
            checkSatWithTimeout (Just timeout)
        satResult <- case r of
            Nothing -> return Nothing
            Just (Unknown msg) -> throwReason $ GraphSliceSolverAnsweredUnknown (CtxSolverConfigOnline config) msg
            Just Sat -> return $ Just True
            Just Unsat -> return $ Just False
        case satResult of
            Just sat -> do
                liftPure $ #ctx % #haveModel .= sat
                when (not sat) $ do
                    let assertion = SMTProofCheckCommandAssert $ SMTProofCheckAssertion $ notS (andNS split)
                    liftPure $ #commands %= (++ [assertion])
                    useCtxM $ \modelConfig -> do
                        sendSimpleCommandExpectingSuccess $ Pop 1
                        sendSimpleCommandExpectingSuccess $ configureCommand modelConfig assertion
                return $ not sat
            Nothing -> do
                modelCtxOpt <- par
                case modelCtxOpt of
                    Nothing -> do
                        returnToOnline
                        let assertion = SMTProofCheckCommandAssert $ SMTProofCheckAssertion $ notS (andNS split)
                        liftPure $ #commands %= (++ [assertion])
                        useCtxM $ \modelConfig -> do
                            sendSimpleCommandExpectingSuccess $ Pop 1
                            sendSimpleCommandExpectingSuccess $ configureCommand modelConfig assertion
                        return True
                    Just satCtx -> do
                        liftPure $ #ctx .= (satCtx & #haveModel .~ True)
                        return False
      where
        split = splitHyp (notS hyp)
        throwReason reason =
            GraphSliceSolverInteractParallel $ throwError $ GraphSliceSolverInteractParallelFailureInfo
                { reason
                }
        par :: GraphSliceSolverInteractParallel m (Maybe Ctx)
        par = do
            solversConfig <- liftPure $ gview $ #solversConfig % #offline
            commands <- liftPure $ use #commands
            let configs = offlineSolverConfigsForSingleCheck solversConfig
            ctxs <- lift $ for configs $ \config -> initCtx $ CtxSolverConfigOffline config
            r <- lift $ forConcurrentlyUnliftIOE (zip [0..] ctxs) $ \(i, ctx) -> do
                useCtx ctx $ \modelConfig -> do
                    for_ commands $ \s -> do
                        sendSimpleCommandExpectingSuccess $ configureCommand modelConfig s
                rs <- useCtx ctx $ \modelConfig -> do
                    sendSimpleCommandExpectingSuccess $ Push 1
                    for_ split $ \s -> do
                        sendSimpleCommandExpectingSuccess $ Assert $ Assertion $ configureSExpr modelConfig s
                    checkSatWithTimeout (Just (ctx ^. #config % expecting #_CtxSolverConfigOffline % #timeout))
                case rs of
                    Nothing -> return $ Right ()
                    Just (Unknown msg) -> return $ Left (Left (GraphSliceSolverAnsweredUnknown ctx.config msg))
                    Just Sat -> return $ Left (Right (Just i))
                    Just Unsat -> return $ Left (Right Nothing)
            case r of
                Right _ -> throwReason $ GraphSliceSolverTimedOut
                Left (Left reason) -> throwReason reason
                Left (Right satOpt) -> do
                    for_ (zip [0..] ctxs) $ \(i, ctx) -> do
                        when (satOpt /= Just i) $ do
                            lift $ liftResourceT $ release ctx.releaseKey
                    return $ satOpt <&> \i -> ctxs !! i

instance (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m, MonadLoggerWithContext m) => MonadGraphSliceGetSExprValue (GraphSliceSolverInteractParallel m) where
    getSExprValue s = do
        haveModel <- liftPure $ use $ #ctx % #haveModel
        ensureM haveModel
        useCtxM $ \modelConfig -> do
            r <- getValue [configureSExpr modelConfig s]
            let [value] = r
            return value

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
