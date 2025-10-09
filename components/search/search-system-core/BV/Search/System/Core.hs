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

import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (lift)
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
    (r, elapsed) <- time $ runExceptT $ flip evalStateT 0 $ discoverInlineScript (runSolverSimple config) input
    let msg = case r of
            Right script -> "discovered script of length " ++ show (length script)
            Left failure -> "failed with " ++ show failure
    logDebug $ msg ++ makeElapsedSuffix elapsed
    return r

discoverStackBounds'
    :: (MonadLoggerWithContext m, MonadUnliftIO m, MonadLogger m, MonadMask m)
    => OnlineSolverConfig -> DiscoverStackBoundsInput -> m (Either GraphSliceSolverInteractSimpleFailureInfo StackBounds)
discoverStackBounds' config input = do
    (r, elapsed) <- time $ runExceptT $ flip evalStateT 0 $ discoverStackBounds (runSolverSimple config) input
    let msg = case r of
            Right _ -> "discovered bounds"
            Left failure -> "failed with " ++ show failure
    logDebug $ msg ++ makeElapsedSuffix elapsed
    return r

runSolverSimple
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverConfig
    -> GraphSliceSolverInteractSimple (SolverT m) a
    -> StateT Integer (ExceptT GraphSliceSolverInteractSimpleFailureInfo m) a
runSolverSimple config m = do
    i <- simple <<%= (+ 1)
    withPushLogContext ("solver run " ++ show i) $ do
        lift $ ExceptT $ runGraphSliceSolverInteractSimple' config m

-- TODO unify with other def
makeElapsedSuffix :: Elapsed -> String
makeElapsedSuffix elapsed = printf " (%.2fs)" (fromRational (elapsedToSeconds elapsed) :: Double)

-- TODO unify with other def
solverProc :: SolverCommand -> CreateProcess
solverProc cmd = proc cmd.path cmd.args
