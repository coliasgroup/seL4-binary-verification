module BV.System.Core.Search
    ( discoverInlineScript'
    , runRepGraphSolverInteractSimple'
    ) where

import BV.Core.Types
import BV.Logging
import BV.Search
import BV.Search.Solver
import BV.SMTLIB2.Process
import BV.System.Core.Solvers
import BV.System.Core.Utils.Logging

import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Unlift (MonadUnliftIO)
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
discoverInlineScript' config input =
    runExceptT $ discoverInlineScript run input
  where
    run :: RepGraphSolverInteractSimple (SolverT m) a -> ExceptT RepGraphSolverInteractSimpleFailureReason m a
    run m = ExceptT $ runRepGraphSolverInteractSimple' config m

-- TODO unify with other def
solverProc :: SolverCommand -> CreateProcess
solverProc cmd = proc cmd.path cmd.args
