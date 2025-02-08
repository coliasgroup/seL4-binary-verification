module BV.CLI.Commands.ExtractSMT
    ( runExtractSMT
    ) where

import BV.CLI.Opts
import BV.System.Utils.Logger

import Control.Monad.IO.Class (MonadIO)

runExtractSMT :: (MonadIO m, MonadLoggerWithContext m) => GlobalOpts -> ExtractSMTOpts -> m ()
runExtractSMT globalOpts extractSMTOpts = do
    return ()
