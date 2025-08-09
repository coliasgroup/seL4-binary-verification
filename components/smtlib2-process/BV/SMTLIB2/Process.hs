{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}

module BV.SMTLIB2.Process
    ( SolverContext (..)
    , SolverProcessException (..)
    , SolverT (..)
    , runSolver
    , runSolverWith
    ) where

import BV.SMTLIB2
import BV.SMTLIB2.SExpr.Build
import BV.SMTLIB2.SExpr.Parse.Attoparsec

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, bracket, finally, throwIO)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, try)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO), toIO)
import Control.Monad.Reader (MonadTrans (lift), ReaderT, asks, runReaderT)
import Data.Conduit (Flush (Chunk, Flush), runConduit, yield, (.|))
import Data.Conduit.Attoparsec (conduitParser)
import qualified Data.Conduit.List as CL
import Data.Conduit.Process (FlushInput (FlushInput),
                             closeStreamingProcessHandle, streamingProcess,
                             streamingProcessHandleRaw, terminateProcess,
                             waitForStreamingProcess)
import Data.Conduit.Text (decodeUtf8)
import qualified Data.Conduit.Text as CT
import Data.Foldable (asum)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Void (absurd)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.Exit (ExitCode)
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
      { sendSExpr :: SExpr -> m ()
      , recvSExprWithTimeout :: Maybe SolverTimeout -> m (Maybe SExpr)
      }

instance Monad m => MonadSolver (SolverT m) where
    sendSExpr req = SolverT $ do
        f <- asks (.sendSExpr)
        lift $ f req
    recvSExprWithTimeout timeout = SolverT $ do
        f <- asks (.recvSExprWithTimeout)
        lift $ f timeout

liftIOContext :: MonadIO m => SolverContext IO -> SolverContext m
liftIOContext ctx = SolverContext
    { sendSExpr = liftIO . ctx.sendSExpr
    , recvSExprWithTimeout = liftIO . ctx.recvSExprWithTimeout
    }

runSolver
    :: (MonadIO m, MonadUnliftIO m, MonadThrow m, MonadMask m)
    => (T.Text -> m ()) -> CreateProcess -> SolverT m a -> m a
runSolver = runSolverWith id

runSolverWith
    :: (MonadIO m, MonadUnliftIO m, MonadThrow m, MonadMask m)
    => (SolverContext m -> SolverContext m) -> (T.Text -> m ()) -> CreateProcess -> SolverT m a -> m a
runSolverWith modifyCtx stderrSink cmd m = withRunInIO $ \run -> bracket
    (streamingProcess cmd)
    cleanup
    (run . go)
  where
    cleanup (_, _, _, sph) =
        closeStreamingProcessHandle sph `finally` terminateProcess (streamingProcessHandleRaw sph)

    go (FlushInput procStdin, procStdout, procStderr, processHandle) = do
        sourceChan <- liftIO newTChanIO

        let sexprToChunks sexpr = map T.encodeUtf8 (TL.toChunks (TB.toLazyText (buildSExpr sexpr <> "\n")))

        let sink sexpr = runConduit $
                (traverse (yield . Chunk) (sexprToChunks sexpr) >> yield Flush)
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
                .| CL.mapM_ stderrSink

        let termination = waitForStreamingProcess processHandle

        let ctx = SolverContext
                { sendSExpr = sink
                , recvSExprWithTimeout = \maybeTimeout ->
                    readTChanWithTimeout (solverTimeoutToMicroseconds <$> maybeTimeout) sourceChan
                }

        let env = asum $ map (Concurrently . (>>= throwIO))
                [ SolverProcessExceptionSource <$> try source
                , SolverProcessExceptionLogging <$> try logging
                , SolverProcessExceptionTermination <$> termination
                ]

        let interaction = runSolverT m (modifyCtx (liftIOContext ctx))

        withRunInIO $ \run -> liftIO $ runConcurrently $ Concurrently (run interaction) <|> (absurd <$> env)

data SolverProcessException
  = SolverProcessExceptionSource (Either SomeException ())
  | SolverProcessExceptionLogging (Either SomeException ())
  | SolverProcessExceptionTermination ExitCode
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
solverTimeoutToMicroseconds timeout = fromIntegerChecked (solverTimeoutToSeconds timeout * 10^6)

fromIntegerChecked :: forall a. HasCallStack => (Num a, Integral a, Bounded a) => Integer -> a
fromIntegerChecked x = if lo <= x && x <= hi then fromInteger x else error "out of bounds"
  where
    lo = toInteger (minBound :: a)
    hi = toInteger (maxBound :: a)
