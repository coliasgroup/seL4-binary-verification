module BV.System.Utils
    ( prependToLogs
    ) where
import Control.Monad.Logger (LoggingT (runLoggingT),
                             MonadLoggerIO (askLoggerIO), ToLogStr (toLogStr))

prependToLogs :: (ToLogStr s, MonadLoggerIO m) => s -> LoggingT m a -> m a
prependToLogs prefix m = do
    logger <- askLoggerIO
    runLoggingT m $
        \loc source level str ->
            logger loc source level (toLogStr prefix <> str)
