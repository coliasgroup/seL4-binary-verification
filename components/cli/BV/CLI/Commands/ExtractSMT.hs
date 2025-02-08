module BV.CLI.Commands.ExtractSMT
    ( runExtractSMT
    ) where

import BV.CLI.Opts
import BV.Logging

import Control.Monad.IO.Class (MonadIO)

runExtractSMT :: (MonadIO m, MonadLoggerWithContext m) => ExtractSMTOpts -> m ()
runExtractSMT extractSMTOpts = do
    return ()
