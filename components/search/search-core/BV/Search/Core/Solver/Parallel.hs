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
import BV.System.Utils.Stopwatch (Elapsed, elapsedToSeconds, time)
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
import Data.List (genericLength)
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

newtype ThrottleN
  = ThrottleN { unwrap :: ThrottleNFn }

type ThrottleNFn = forall m a. MonadUnliftIO m => Integer -> m a -> m a

data ParallelEnv m
  = ParallelEnv
      { solversConfig :: SolversConfig
      , throttleN :: ThrottleN
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
  where
    -- TODO unify with other def
    solverProc :: SolverCommand -> CreateProcess
    solverProc cmd = proc cmd.path cmd.args

withPushSolverConfigLogContext :: MonadLoggerWithContext m => CtxSolverConfig -> m a -> m a
withPushSolverConfigLogContext = \case
    CtxSolverConfigOnline _ ->
        withPushLogContext "online"
    CtxSolverConfigOffline config ->
        withPushLogContext "offline" .
            withPushLogContext ("solver " ++ config.commandName ++ " " ++ prettyModelConfig config.modelConfig)

liftPure :: Monad m => StateT (ParallelState m) (Reader ((ParallelEnv m))) a -> GraphSliceSolverInteractParallel m a
liftPure = GraphSliceSolverInteractParallel . lift . mapStateT (mapReaderT (return . runIdentity))

liftInner :: Monad m => m a -> GraphSliceSolverInteractParallel m a
liftInner = GraphSliceSolverInteractParallel . lift . lift . lift

instance MonadTrans GraphSliceSolverInteractParallel where
    lift = GraphSliceSolverInteractParallel . lift . lift . lift

data GraphSliceSolverInteractParallelFailureInfo
  = GraphSliceSolverInteractParallelFailureInfo
      { reason :: GraphSliceSolverInteractParallelFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

data GraphSliceSolverInteractParallelFailureReason
  = GraphSliceSolverInteractParallelFailureReasonTimedOut
  | GraphSliceSolverInteractParallelFailureReasonAnsweredUnknown GraphSliceSolverInteractParallelSolverId SExpr
  deriving (Eq, Generic, Ord, Show)

data GraphSliceSolverInteractParallelSolverId
  = GraphSliceSolverInteractParallelSolverIdOnline
  | GraphSliceSolverInteractParallelSolverIdOffline String ModelConfig
  deriving (Eq, Generic, Ord, Show)

runGraphSliceSolverInteractParallel
    :: (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m, MonadLoggerWithContext m)
    => (forall n b. MonadUnliftIO n => Integer -> n b -> n b) -> SolversConfig -> GraphSliceSolverInteractParallel m a -> m (Either GraphSliceSolverInteractParallelFailureInfo a)
runGraphSliceSolverInteractParallel throttleN solversConfig m = throttleN 1 $ do
    ctx <- initCtx (CtxSolverConfigOnline (fromJust solversConfig.online))
    let initState = ParallelState
            { commands = []
            , ctx
            }
    runReaderT (evalStateT (runExceptT m.run) initState) env
  where
    env = ParallelEnv
        { solversConfig
        , throttleN = ThrottleN throttleN
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
    let stderrSink =
            run . withPushSolverConfigLogContext config . withPushLogContext "stderr" . logInfoGeneric
    (releaseKey, ctx) <- allocateAcquire $ acquireSolverContext stderrSink (procOf config)
    let this = Ctx
            { ctx
            , releaseKey = releaseKey
            , config
            , haveModel = False
            }
    useCtx this $ \modelConfig -> do
        sendSimpleCommandExpectingSuccess $ SetOption $ PrintSuccessOption True
        sendSimpleCommandExpectingSuccess $ SetOption $ ProduceModelsOption True
        sendSimpleCommandExpectingSuccess $ SetLogic defaultLogic
        traverse_ sendExpectingSuccess $ modelConfigPreamble modelConfig
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

onboardCtxtM
   :: ( MonadUnliftIO m
      , MonadThrow m
      , MonadMask m
      , MonadResource m
      , MonadLoggerWithContext m
      )
    => GraphSliceSolverInteractParallel m ()
onboardCtxtM = do
    commands <- liftPure $ use #commands
    onlineConfig <- liftPure $ gview $ #solversConfig % #online % unwrapped
    ctx <- lift $ onboardCtx commands $ CtxSolverConfigOnline onlineConfig
    liftPure $ #ctx .= ctx

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
            onboardCtxtM

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
        satResultOpt <- useCtxM $ \modelConfig -> do
            sendSimpleCommandExpectingSuccess $ Push 1
            for_ split $ \s -> do
                sendSimpleCommandExpectingSuccess $ Assert $ Assertion $ configureSExpr modelConfig s
            checkSatWithTimeout (Just timeout)
        case satResultOpt of
            Just Unsat -> do
                liftPure $ #commands %= (++ [assertion])
                useCtxM $ \modelConfig -> do
                    sendSimpleCommandExpectingSuccess $ Pop 1
                    sendSimpleCommandExpectingSuccess $ configureCommand modelConfig assertion
                return True
            Just Sat -> do
                liftPure $ #ctx % #haveModel .= True
                return False
            Just (Unknown msg) ->
                GraphSliceSolverInteractParallel $
                    throwError $
                        GraphSliceSolverInteractParallelFailureInfo $
                            GraphSliceSolverInteractParallelFailureReasonAnsweredUnknown
                                GraphSliceSolverInteractParallelSolverIdOnline
                                msg
            Nothing -> do
                modelCtxOptE <- do
                    solversConfig <- liftPure $ gview $ #solversConfig % #offline
                    let configs = offlineSolverConfigsForSingleCheck solversConfig
                    commands <- liftPure $ use #commands
                    ThrottleN throttleN <- liftPure $ gview #throttleN
                    liftInner $ throttleN (genericLength configs - 1) $ par configs commands
                case modelCtxOptE of
                    Left err -> GraphSliceSolverInteractParallel $ throwError err
                    Right Nothing -> do
                        liftPure $ #commands %= (++ [assertion])
                        onboardCtxtM
                        return True
                    Right (Just modelCtx) -> do
                        liftPure $ #ctx .= modelCtx
                        liftPure $ #ctx % #haveModel .= True
                        return False
      where
        split = splitHyp (notS hyp)
        assertion = SMTProofCheckCommandAssert $ SMTProofCheckAssertion $ notS (andNS split)
        par :: [OfflineSolverConfig] -> [SMTProofCheckCommand] -> m (Either GraphSliceSolverInteractParallelFailureInfo (Maybe Ctx))
        par configs commands = do
            ctxs <- for configs $ \config -> initCtx $ CtxSolverConfigOffline config
            r <- forConcurrentlyUnliftIOE (zip [0..] ctxs) $ \(i, ctx) -> do
                (satResult, elapsed) <- time $ useCtx ctx $ \modelConfig -> do
                    for_ commands $ \s -> do
                        sendSimpleCommandExpectingSuccess $ configureCommand modelConfig s
                    for_ split $ \s -> do
                        sendSimpleCommandExpectingSuccess $ Assert $ Assertion $ configureSExpr modelConfig s
                    let timeout = ctx ^. #config % expecting #_CtxSolverConfigOffline % #timeout
                    checkSatWithTimeout (Just timeout)
                logOfflineSolverResult satResult elapsed
                case satResult of
                    Nothing -> return $ Right ()
                    Just (Unknown msg) ->
                        let offlineConfig = ctx ^. #config % expecting #_CtxSolverConfigOffline
                            solverId = GraphSliceSolverInteractParallelSolverIdOffline
                                offlineConfig.commandName
                                offlineConfig.modelConfig
                         in return $ Left $ Left $
                                GraphSliceSolverInteractParallelFailureReasonAnsweredUnknown solverId msg
                    Just Sat -> return $ Left $ Right (Just i)
                    Just Unsat -> return $ Left $ Right Nothing
            case r of
                Right _ -> return $ Left $ GraphSliceSolverInteractParallelFailureInfo $ GraphSliceSolverInteractParallelFailureReasonTimedOut
                Left (Left reason) -> return $ Left $ GraphSliceSolverInteractParallelFailureInfo reason
                Left (Right satIxOpt) -> Right <$> do
                    for_ (zip [0..] ctxs) $ \(i, ctx) -> do
                        when (satIxOpt /= Just i) $ do
                            liftResourceT $ release ctx.releaseKey
                    return $ satIxOpt <&> \i -> ctxs !! i

instance (MonadUnliftIO m, MonadThrow m, MonadMask m, MonadResource m, MonadLoggerWithContext m) => MonadGraphSliceGetSExprValue (GraphSliceSolverInteractParallel m) where
    getSExprValue s = do
        haveModel <- liftPure $ use $ #ctx % #haveModel
        ensureM haveModel
        useCtxM $ \modelConfig -> do
            r <- getValue [configureSExpr modelConfig s]
            let [value] = r
            return value

-- TODO unify with other def
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

-- TODO unify with other def
makeElapsedSuffix :: Elapsed -> String
makeElapsedSuffix elapsed = printf " (%.2fs)" (fromRational (elapsedToSeconds elapsed) :: Double)
