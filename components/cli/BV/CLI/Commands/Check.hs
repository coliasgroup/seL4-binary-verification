module BV.CLI.Commands.Check
    ( runCheck
    ) where

import BV.CLI.Opts
import BV.Logging

import Control.Monad.IO.Class (MonadIO)

runCheck :: (MonadIO m, MonadLoggerWithContext m) => CheckOpts -> m ()
runCheck checkOpts = do
    return ()
