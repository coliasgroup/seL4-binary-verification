module BV.System.Search.Core
    ( discoverInlineScript'
    , discoverStackBounds'
    , runRepGraphSolverInteractSimple'
    ) where

import BV.Core.Types
import BV.Logging
import BV.Search.Core
import BV.Search.Core.Solver
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

runRepGraphSolverInteractSimple'
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverConfig -> RepGraphSolverInteractSimple (SolverT m) a -> m (Either RepGraphSolverInteractSimpleFailureInfo a)
runRepGraphSolverInteractSimple' config m =
    runSolverWithLogging
        (solverProc config.command)
        (runRepGraphSolverInteractSimple (Just config.timeout) config.modelConfig m)

discoverInlineScript'
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverConfig -> DiscoverInlineScriptInput -> m (Either RepGraphSolverInteractSimpleFailureInfo InlineScript')
discoverInlineScript' config input = withPushLogContextPairing input.pairingId $ do
    logDebug "searching"
    (r, elapsed) <- time $ runExceptT $ flip evalStateT 0 $ discoverInlineScript (runSolverSimple config) input
    let msg = case r of
            Right script -> "discovered script of length " ++ show (length script)
            Left failure -> "failed with " ++ show failure
    logDebug $ msg ++ makeElapsedSuffix elapsed
    return r

discoverStackBounds'
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverConfig -> DiscoverStackBoundsInput -> m (Either RepGraphSolverInteractSimpleFailureInfo StackBounds)
discoverStackBounds' config input = do
    logDebug "searching"
    (r, elapsed) <- time $ runExceptT $ flip evalStateT 0 $ discoverStackBounds (runSolverSimple config) input
    let msg = case r of
            Right _ -> "discovered bounds"
            Left failure -> "failed with " ++ show failure
    logDebug $ msg ++ makeElapsedSuffix elapsed
    return r

runSolverSimple
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverConfig
    -> RepGraphSolverInteractSimple (SolverT m) a
    -> StateT Integer (ExceptT RepGraphSolverInteractSimpleFailureInfo m) a
runSolverSimple config m = do
    i <- simple <<%= (+ 1)
    withPushLogContext ("solver run " ++ show i) $ do
        lift $ ExceptT $ runRepGraphSolverInteractSimple' config m

-- TODO unify with other def
makeElapsedSuffix :: Elapsed -> String
makeElapsedSuffix elapsed = printf " (%.2fs)" (fromRational (elapsedToSeconds elapsed) :: Double)

-- TODO unify with other def
solverProc :: SolverCommand -> CreateProcess
solverProc cmd = proc cmd.path cmd.args
