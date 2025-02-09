module BV.Logging.Adapters
    ( runSimpleLoggingWithContextT
    ) where

import BV.Logging.Formatting
import BV.Logging.Types

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT (LoggingT))

runSimpleLoggingWithContextT :: MonadIO m => LoggingWithContextT m a -> LoggingT m a
runSimpleLoggingWithContextT m = LoggingT $ \logAction ->
    runLoggingWithContextT m (adaptLogEntryFormatterWith False logAction)
