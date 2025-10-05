module BV.SMTLIB2.Process
    ( SolverContext (..)
    , SolverProcessException (..)
    , SolverT (..)
    , mapSolverContext
    , monitorSolverContext
    , monitoringSolverContext
    , runSolver
    , runSolverT
    , runSolverWith
    ) where

import BV.SMTLIB2 (MonadSolver (..), SExpr, SolverTimeout,
                   solverTimeoutToSeconds)
import BV.SMTLIB2.SExpr.Build (buildSExpr)
import BV.SMTLIB2.SExpr.Parse.Attoparsec (consumeSomeSExprWhitespace,
                                          parseSExpr)

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently),
                                 async, uninterruptibleCancel)
import Control.Concurrent.STM (STM, TChan, atomically, check, newEmptyTMVarIO,
                               newTChanIO, putTMVar, readTChan, readTMVar,
                               readTVar, registerDelay, writeTChan)
import Control.Exception (Exception, SomeException, finally)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, throwM, try)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Control.Monad.Reader (MonadTrans (lift), ReaderT, asks, runReaderT)
import Data.Acquire (Acquire, mkAcquire, withAcquire)
import Data.Conduit (Flush (Chunk, Flush), runConduit, yield, (.|))
import Data.Conduit.Attoparsec (conduitParser)
import qualified Data.Conduit.List as CL
import Data.Conduit.Process (FlushInput (FlushInput),
                             closeStreamingProcessHandle, streamingProcess,
                             streamingProcessHandleRaw, terminateProcess,
                             waitForStreamingProcess)
import Data.Conduit.Text (decodeUtf8)
import qualified Data.Conduit.Text as CT
import Data.Foldable (asum, traverse_)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import GHC.Generics (Generic)
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

instance MonadTrans SolverT where
    lift = SolverT . lift

runSolverT :: Monad m => SolverT m a -> SolverContext m -> m a
runSolverT = runReaderT . (.unwrap)

data SolverContext m
  = SolverContext
      { sendSExpr :: SExpr -> m ()
      , recvSExprWithTimeout :: Maybe SolverTimeout -> m (Maybe SExpr)
      , monitor :: STM SolverProcessException
      }

mapSolverContext :: (forall a. m a -> n a) -> SolverContext m -> SolverContext n
mapSolverContext f ctx = SolverContext
    { sendSExpr = f . ctx.sendSExpr
    , recvSExprWithTimeout = f . ctx.recvSExprWithTimeout
    , monitor = ctx.monitor
    }

instance Monad m => MonadSolver (SolverT m) where
    sendSExpr req = SolverT $ do
        f <- asks (.sendSExpr)
        lift $ f req
    recvSExprWithTimeout timeout = SolverT $ do
        f <- asks (.recvSExprWithTimeout)
        lift $ f timeout

monitorSolverContext :: SolverContext m -> IO a
monitorSolverContext ctx = atomically ctx.monitor >>= throwM

monitoringSolverContext :: SolverContext m -> IO a -> IO a
monitoringSolverContext ctx m = runConcurrently $
    Concurrently m <|> Concurrently (monitorSolverContext ctx)

acquireSolverContext :: (T.Text -> IO ()) -> CreateProcess -> Acquire (SolverContext IO)
acquireSolverContext stderrSink cmd = do

    (FlushInput procStdin, procStdout, procStderr, processHandle) <-
        mkAcquire (streamingProcess cmd) $ \(_, _, _, sph) -> do
            closeStreamingProcessHandle sph
                `finally` terminateProcess (streamingProcessHandleRaw sph)

    sourceChan <- liftIO $ newTChanIO

    exceptionSlot <- liftIO $ newEmptyTMVarIO

    let sexprToChunks sexpr =
            map T.encodeUtf8 (TL.toChunks (TB.toLazyText (buildSExpr sexpr <> TB.fromString "\n")))

        sink sexpr = runConduit $
            (traverse_ (yield . Chunk) (sexprToChunks sexpr) >> yield Flush)
            .| procStdin

        source = runConduit $
            procStdout
            .| decodeUtf8
            .| conduitParser (Just <$> parseSExpr <|> Nothing <$ consumeSomeSExprWhitespace)
            .| CL.map snd
            .| CL.concat
            .| CL.mapM_ (atomically . writeTChan sourceChan)

        logging = runConduit $
            procStderr
            .| decodeUtf8
            .| CT.lines
            .| CL.mapM_ stderrSink

        termination = waitForStreamingProcess processHandle

        ctx = SolverContext
            { sendSExpr = sink
            , recvSExprWithTimeout = \maybeTimeout ->
                readTChanWithTimeout (solverTimeoutToMicroseconds <$> maybeTimeout) sourceChan
            , monitor = readTMVar exceptionSlot
            }

        monitorBackend = do
            ex <- runConcurrently $ asum $ map Concurrently
                [ SolverProcessExceptionSource <$> try source
                , SolverProcessExceptionLogging <$> try logging
                , SolverProcessExceptionTermination <$> termination
                ]
            atomically $ putTMVar exceptionSlot ex

    mkAcquire (async monitorBackend) uninterruptibleCancel

    return $ mapSolverContext (monitoringSolverContext ctx) ctx

runSolver
    :: (MonadIO m, MonadUnliftIO m, MonadThrow m, MonadMask m)
    => (T.Text -> m ()) -> CreateProcess -> SolverT m a -> m a
runSolver = runSolverWith id

runSolverWith
    :: (MonadIO m, MonadUnliftIO m, MonadThrow m, MonadMask m)
    => (SolverContext m -> SolverContext m) -> (T.Text -> m ()) -> CreateProcess -> SolverT m a -> m a
runSolverWith modifyCtx stderrSink cmd m = withRunInIO $ \run ->
    withAcquire (acquireSolverContext (run . stderrSink) cmd) $ \ctx ->
        monitoringSolverContext ctx $
            run $
                runSolverT m (modifyCtx (mapSolverContext liftIO ctx))

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
solverTimeoutToMicroseconds timeout = fromIntegerChecked (solverTimeoutToSeconds timeout * 10^(6 :: Integer))

fromIntegerChecked :: forall a. (Num a, Integral a, Bounded a) => Integer -> a
fromIntegerChecked x = if lo <= x && x <= hi then fromInteger x else error "out of bounds"
  where
    lo = toInteger (minBound :: a)
    hi = toInteger (maxBound :: a)
