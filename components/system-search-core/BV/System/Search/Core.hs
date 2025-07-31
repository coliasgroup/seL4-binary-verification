module BV.System.Search.Core
    ( discoverInlineScript'
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

import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (lift)
import Optics
import Optics.State.Operators ((<<%=))
import System.Process (CreateProcess, proc)

runRepGraphSolverInteractSimple'
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverConfig -> RepGraphSolverInteractSimple (SolverT m) a -> m (Either RepGraphSolverInteractSimpleFailureReason a)
runRepGraphSolverInteractSimple' config m =
    runSolverWithLogging
        (solverProc config.command)
        (runRepGraphSolverInteractSimple (Just config.timeout) config.modelConfig m)

discoverInlineScript'
    :: forall m. (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverConfig -> DiscoverInlineScriptInput -> m (Either RepGraphSolverInteractSimpleFailureReason InlineScript')
discoverInlineScript' config input = withPushLogContextPairing input.pairingId $ do
    logDebug "searching"
    r <- runExceptT $ flip evalStateT 0 $ discoverInlineScript run input
    case r of
        Right script -> logDebug $ "discovered script of length " ++ show (length script)
        Left failure -> logDebug $ "failed with " ++ show failure
    return r
  where
    run :: RepGraphSolverInteractSimple (SolverT m) a -> StateT Integer (ExceptT RepGraphSolverInteractSimpleFailureReason m) a
    run m = do
        i <- simple <<%= (+ 1)
        withPushLogContext ("solver run " ++ show i) $ do
            lift $ ExceptT $ runRepGraphSolverInteractSimple' config m

-- TODO unify with other def
solverProc :: SolverCommand -> CreateProcess
solverProc cmd = proc cmd.path cmd.args
