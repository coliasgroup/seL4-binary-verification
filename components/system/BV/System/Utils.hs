module BV.System.Utils
    ( addLogContext
    , addLogContext'
    , prependToLogs
    , prependToLogs'
    ) where

import Control.Monad.Logger (LoggingT (LoggingT, runLoggingT),
                             MonadLoggerIO (askLoggerIO), ToLogStr (toLogStr))

prependToLogs :: String -> LoggingT m a -> LoggingT m a
prependToLogs prefix m =
    LoggingT $ \logger ->
        runLoggingT m $ \loc source level str ->
            logger loc source level (toLogStr prefix <> str)

prependToLogs' :: MonadLoggerIO m => String -> LoggingT m a -> m a
prependToLogs' prefix m = do
    logger <- askLoggerIO
    runLoggingT m $
        \loc source level str ->
            logger loc source level (toLogStr prefix <> str)

addLogContext :: String -> LoggingT m a -> LoggingT m a
addLogContext prefix = prependToLogs $ "[" ++ prefix ++ "] "

addLogContext' :: MonadLoggerIO m => String -> LoggingT m a -> m a
addLogContext' prefix = prependToLogs' $ "[" ++ prefix ++ "] "
