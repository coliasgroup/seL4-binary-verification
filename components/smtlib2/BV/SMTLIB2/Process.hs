{-# LANGUAGE OverloadedStrings #-}

module BV.SMTLIB2.Process
    ( SolverT
    , runSolver
    ) where

import BV.SMTLIB2.Builder
import BV.SMTLIB2.Parser.Attoparsec
import BV.SMTLIB2.Types

import Control.Applicative ((<|>))
import Control.Concurrent.Async (withAsync)
import Control.Exception (Exception)
import Control.Exception.Base (throw)
import Control.Monad.Catch (MonadThrow, finally)
import Control.Monad.Catch.Pure (MonadMask)
import Control.Monad.Free (liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Control.Monad.Reader (MonadTrans (lift), ReaderT (..), ask, runReaderT)
import Control.Monad.Trans.Free.Church (FT, iterT)
import Data.Conduit (ConduitT, Flush (Chunk, Flush), await, runConduit, yield,
                     (.|))
import Data.Conduit.Attoparsec (conduitParser)
import qualified Data.Conduit.List as CL
import Data.Conduit.Process (FlushInput (FlushInput), streamingProcess,
                             streamingProcessHandleRaw, terminateProcess)
import Data.Conduit.Text (decodeUtf8)
import qualified Data.Conduit.Text as CT
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Void (Void)
import GHC.Generics (Generic)
import System.Process (CreateProcess)

data SolverDSL a
  = Send SExpr a
  | Recv (SExpr -> a)
  deriving (Functor, Generic)

instance Monad m => MonadSolver (FT SolverDSL m) where
    send msg = liftF $ Send msg ()
    recv = liftF $ Recv id

type SolverT m = FT SolverDSL (SolverTInner m)

type SolverTInner m = ReaderT (SExpr -> m ()) (ConduitT SExpr Void m)

runSolverT :: Monad m => SolverT m a -> SolverTInner m a
runSolverT = iterT $ \case
    Send msg m -> do
        sink <- ask
        lift . lift $ sink msg
        m
    Recv f -> lift await >>= f . fromMaybe (throw NoResponseException)

runSolver
    :: (MonadIO m, MonadUnliftIO m, MonadThrow m, MonadMask m)
    => CreateProcess -> (T.Text -> m ()) -> SolverT m a -> m a
runSolver cmd logStderr m = do
    ((FlushInput procStdin, closeProcStdin), procStdout, procStderr, cph) <- liftIO $ streamingProcess cmd

    let sexprToChunks sexpr = map T.encodeUtf8 (TL.toChunks (TB.toLazyText (buildSExpr sexpr <> "\n")))

    let sink sexpr = runConduit $
               (mapM_ (yield . Chunk) (sexprToChunks sexpr) >> yield Flush)
            .| procStdin

    let interaction =
               procStdout
            .| decodeUtf8
            .| conduitParser (Just <$> parseSExpr <|> Nothing <$ consumeSomeSExprWhitespace)
            .| CL.map snd
            .| CL.concat
            .| (runReaderT (runSolverT m) sink <* closeProcStdin)

    let logging = runConduit $
               procStderr
            .| decodeUtf8
            .| CT.lines
            .| CL.mapM_ logStderr

    let run = withRunInIO $ \f -> liftIO . withAsync (f logging) $ \_ -> f (runConduit interaction)

    run `finally` liftIO (terminateProcess (streamingProcessHandleRaw cph))

data NoResponseException
  = NoResponseException
  deriving (Show)

instance Exception NoResponseException
