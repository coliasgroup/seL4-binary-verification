module BV.System.Core.Search
    ( discoverInlineScript'
    , runSimpleSolver'
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

runSimpleSolver'
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverConfig -> SimpleSolver (SolverT m) a -> m (Either SimpleSolverFailureReason a)
runSimpleSolver' config m =
    runSolverWithLogging
        (solverProc config.command)
        (runSimpleSolver (Just config.timeout) config.modelConfig m)

discoverInlineScript'
    :: forall m. (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
    => OnlineSolverConfig -> DiscoverInlineScriptInput -> m (Either SimpleSolverFailureReason InlineScript')
discoverInlineScript' config input =
    runExceptT $ discoverInlineScript run input
  where
    run :: SimpleSolver (SolverT m) a -> ExceptT SimpleSolverFailureReason m a
    run m = ExceptT $ runSimpleSolver' config m

-- TODO unify with other def
solverProc :: SolverCommand -> CreateProcess
solverProc cmd = proc cmd.path cmd.args
