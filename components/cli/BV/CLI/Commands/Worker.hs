module BV.CLI.Commands.Worker
    ( runWorker
    ) where

import BV.CLI.Opts

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)

runWorker :: (MonadThrow m, MonadIO m) => WorkerOpts -> m ()
runWorker opts = do
    undefined
