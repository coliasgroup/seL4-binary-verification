module BV.Logging.Adapters
    ( runSimpleLoggingTWithContext
    , runSimpleLoggingWithContextT
    ) where

import BV.Logging.Formatting
import BV.Logging.Types

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT (LoggingT, runLoggingT))
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)

runSimpleLoggingWithContextT :: MonadIO m => LoggingWithContextT m a -> LoggingT m a
runSimpleLoggingWithContextT m = LoggingT $ \logAction ->
    runLoggingWithContextT m (adaptLogEntryFormatterWith False logAction)

runSimpleLoggingTWithContext :: MonadIO m => LoggingT m a -> LoggingWithContextT m a
runSimpleLoggingTWithContext m = LoggingWithContextT $ do
    env <- ask
    lift $ runLoggingT m $ \loc source level msg -> env.logAction (LogEntry
        { context = env.context
        , loc
        , source
        , level
        , msg
        })
