{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}

module BV.SMTLIB2.Process
    ( SolverContext (..)
    , SolverProcessException (..)
    , SolverT
    , runSolver
    , runSolverWith
    ) where

import BV.SMTLIB2.Builder
import BV.SMTLIB2.Parser.Attoparsec
import BV.SMTLIB2.Types

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently),
                                 cancelWith, wait, withAsync)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException)
import Control.Monad.Catch (MonadCatch, MonadThrow, finally, try)
import Control.Monad.Catch.Pure (MonadMask)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, toIO)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (..), runReaderT)
import Control.Monad.Reader.Class (asks)
import Data.Conduit (Flush (Chunk, Flush), runConduit, yield, (.|))
import Data.Conduit.Attoparsec (conduitParser)
import qualified Data.Conduit.List as CL
import Data.Conduit.Process (FlushInput (FlushInput), streamingProcess,
                             streamingProcessHandleRaw, terminateProcess,
                             waitForStreamingProcess)
import Data.Conduit.Text (decodeUtf8)
import qualified Data.Conduit.Text as CT
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import GHC.Generics (Generic)
import System.Process (CreateProcess)

newtype SolverT m a
  = SolverT { unwrap :: SolverTInner m a }
  deriving
    ( Applicative
    , Functor
    , Generic
    , Monad
    , MonadCatch
    , MonadError e
    , MonadFail
    , MonadIO
    , MonadMask
    , MonadThrow
    , MonadUnliftIO
    )

type SolverTInner m = ReaderT (SolverContext m) m

runSolverT :: Monad m => SolverT m a -> SolverContext m -> m a
runSolverT = runReaderT . (.unwrap)

data SolverContext m
  = SolverContext
      { send :: SExpr -> m ()
      , recvWithTimeout :: Maybe SolverTimeout -> m (Maybe SExpr)
      }

instance Monad m => MonadSolver (SolverT m) where
    send req = SolverT $ do
        f <- asks (.send)
        lift $ f req
    recvWithTimeout timeout = SolverT $ do
        f <- asks (.recvWithTimeout)
        lift $ f timeout

liftIOContext :: MonadIO m => SolverContext IO -> SolverContext m
liftIOContext ctx = SolverContext
    { send = liftIO . ctx.send
    , recvWithTimeout = liftIO . ctx.recvWithTimeout
    }

runSolver
    :: (MonadIO m, MonadUnliftIO m, MonadThrow m, MonadMask m)
    => CreateProcess -> (T.Text -> m ()) -> SolverT m a -> m a
runSolver = runSolverWith id

runSolverWith
    :: (MonadIO m, MonadUnliftIO m, MonadThrow m, MonadMask m)
    => (SolverContext m -> SolverContext m) -> CreateProcess -> (T.Text -> m ()) -> SolverT m a -> m a
runSolverWith modifyCtx cmd logStderr m = do
    ((FlushInput procStdin, closeProcStdin), procStdout, procStderr, processHandle) <-
        liftIO $ streamingProcess cmd

    sourceChan <- liftIO newTChanIO

    let sexprToChunks sexpr = map T.encodeUtf8 (TL.toChunks (TB.toLazyText (buildSExpr sexpr <> "\n")))

    let sink sexpr = runConduit $
               (mapM_ (yield . Chunk) (sexprToChunks sexpr) >> yield Flush)
            .| procStdin

    let source = runConduit $
               procStdout
            .| decodeUtf8
            .| conduitParser (Just <$> parseSExpr <|> Nothing <$ consumeSomeSExprWhitespace)
            .| CL.map snd
            .| CL.concat
            .| CL.mapM_ (atomically . writeTChan sourceChan)

    logging <- toIO . runConduit $
               procStderr
            .| decodeUtf8
            .| CT.lines
            .| CL.mapM_ logStderr

    let termination = waitForStreamingProcess processHandle

    let ctx = SolverContext
            { send = sink
            , recvWithTimeout = \maybeTimeout ->
                readTChanWithTimeout (solverTimeoutToMicroseconds <$> maybeTimeout) sourceChan
            }

    let env = runConcurrently $
                SolverProcessExceptionSource <$> Concurrently (try source)
            <|> SolverProcessExceptionLogging <$> Concurrently (try logging)
            <|> SolverProcessExceptionTermination <$ Concurrently termination

    interaction <- toIO $ runSolverT m (modifyCtx (liftIOContext ctx))

    let run =
            withAsync env $ \envA ->
                withAsync interaction $ \interactionA ->
                    let propagate = do
                            ex <- wait envA
                            cancelWith interactionA ex
                     in withAsync propagate $ \_ ->
                            wait interactionA

    liftIO $ (run <* closeProcStdin) `finally` terminateProcess (streamingProcessHandleRaw processHandle)

data SolverProcessException
  = SolverProcessExceptionSource (Either SomeException ())
  | SolverProcessExceptionLogging (Either SomeException ())
  | SolverProcessExceptionTermination
  deriving (Generic, Show)

instance Exception SolverProcessException

readTChanWithTimeout :: Maybe Int -> TChan a -> IO (Maybe a)
readTChanWithTimeout maybeMicroseconds chan = case maybeMicroseconds of
    Just microseconds -> do
        delay <- registerDelay microseconds
        atomically $ do
                Just <$> readTChan chan
            <|> Nothing <$ (readTVar delay >>= check)
    Nothing -> Just <$> atomically (readTChan chan)

solverTimeoutToMicroseconds :: SolverTimeout -> Int
solverTimeoutToMicroseconds timeout = fromIntegerChecked (timeout.seconds * 10^6)

fromIntegerChecked :: forall a. (Bounded a, Integral a) => Integer -> a
fromIntegerChecked x =
    if x <= toInteger (maxBound :: a)
    then fromInteger x
    else error "out of range"
