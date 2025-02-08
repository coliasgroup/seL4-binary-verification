module BV.CLI.Commands.Check
    ( runCheck
    ) where

import BV.CLI.Opts
import BV.System.Utils.Logger

import Control.Monad.IO.Class (MonadIO)

runCheck :: (MonadIO m, MonadLoggerWithContext m) => GlobalOpts -> CheckOpts -> m ()
runCheck globalOpts checkOpts = do
    return ()
