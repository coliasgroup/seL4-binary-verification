module BV.Search.System.Core
    ( discoverInlineScript'
    , discoverStackBounds'
    , runGraphSliceSolverInteractSimple'
    ) where

import BV.Core.Types
import BV.Logging
import BV.Search.Core
import BV.Search.Core.Solver.Simple
import BV.SMTLIB2.Process
import BV.System.Core
import BV.System.Core.Utils.Logging (runSolverWithLogging,
                                     withPushLogContextPairing)
import BV.System.Utils.Stopwatch

import BV.System.Utils.UnliftIO.Async (forConcurrentlyUnliftIOE)
import Control.Concurrent (MVar, modifyMVar, newMVar)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), ask, mapReaderT)
import Control.Monad.State (State, runState)
import Control.Monad.Trans (lift)
import Data.Tuple (swap)
import Optics
import Optics.State.Operators ((<<%=))
import System.Process (CreateProcess, proc)
import Text.Printf (printf)

runGraphSliceSolverInteractSimple'
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverConfig -> GraphSliceSolverInteractSimple (SolverT m) a -> m (Either GraphSliceSolverInteractSimpleFailureInfo a)
runGraphSliceSolverInteractSimple' config m =
    runSolverWithLogging
        (solverProc config.command)
        (runGraphSliceSolverInteractSimple (Just config.timeout) config.modelConfig m)

discoverInlineScript'
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverConfig -> DiscoverInlineScriptInput -> m (Either GraphSliceSolverInteractSimpleFailureInfo InlineScript')
discoverInlineScript' config input = withPushLogContextPairing input.pairingId $ do
    logDebug "searching"
    (r, elapsed) <- time $ runSolverCounterT $ runExceptT $ discoverInlineScript (runSolverSimple config) input
    let msg = case r of
            Right script -> "discovered script of length " ++ show (length script)
            Left failure -> "failed with " ++ show failure
    logDebug $ msg ++ makeElapsedSuffix elapsed
    return r

discoverStackBounds'
    :: forall m. (MonadLoggerWithContext m, MonadUnliftIO m, MonadLogger m, MonadMask m)
    => (forall a. m a -> m a) -> OnlineSolverConfig -> DiscoverStackBoundsInput -> m (Either GraphSliceSolverInteractSimpleFailureInfo StackBounds)
discoverStackBounds' throttle config input = do
    (r, elapsed) <- time $ runSolverCounterT $ runExceptT $ discoverStackBounds (runSolverSimple config) forConc input
    let msg = case r of
            Right _ -> "discovered bounds"
            Left failure -> "failed with " ++ show failure
    logDebug $ msg ++ makeElapsedSuffix elapsed
    return r
  where
    forConc
        :: forall t a b. Traversable t
        => t a
        -> (a -> ExceptT GraphSliceSolverInteractSimpleFailureInfo (ReaderT (MVar Integer) m) b)
        -> ExceptT GraphSliceSolverInteractSimpleFailureInfo (ReaderT (MVar Integer) m) (t b)
    forConc xs f = ExceptT $ forConcurrentlyUnliftIOE xs $ \x -> mapReaderT throttle $ runExceptT $ f x

runSolverCounterT :: MonadIO m => ReaderT (MVar Integer) m a -> m a
runSolverCounterT m = do
    counter <- liftIO $ newMVar 0
    runReaderT m counter

atomicState :: (MonadReader (MVar s) m, MonadIO m) => State s a -> m a
atomicState act = do
    v <- ask
    liftIO $ modifyMVar v (return . swap . runState act)

runSolverSimple
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverConfig
    -> GraphSliceSolverInteractSimple (SolverT m) a
    -> ExceptT GraphSliceSolverInteractSimpleFailureInfo (ReaderT (MVar Integer) m) a
runSolverSimple config m = do
    i <- atomicState $ simple <<%= (+ 1)
    withPushLogContext ("solver run " ++ show i) $ do
        ExceptT $ lift $ runGraphSliceSolverInteractSimple' config m

-- TODO unify with other def
makeElapsedSuffix :: Elapsed -> String
makeElapsedSuffix elapsed = printf " (%.2fs)" (fromRational (elapsedToSeconds elapsed) :: Double)

-- TODO unify with other def
solverProc :: SolverCommand -> CreateProcess
solverProc cmd = proc cmd.path cmd.args
