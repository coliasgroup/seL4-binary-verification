{-# LANGUAGE OverloadedStrings #-}

module BV.SMTLIB2.Process
    ( SolverT
    , runSolver
    ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (withAsync)
import Control.Exception (Exception)
import Control.Exception.Base (throw)
import Control.Monad.Catch (MonadThrow, finally)
import Control.Monad.Catch.Pure (MonadMask)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Control.Monad.Reader (MonadTrans (lift), ReaderT (..), ask, runReaderT)
import Data.Conduit (ConduitT, Flush (Chunk, Flush), await, runConduit,
                     toProducer, yield, (.|))
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

import BV.SMTLIB2.Builder
import BV.SMTLIB2.Parser.Attoparsec
import BV.SMTLIB2.Types

newtype SolverT m a
  = SolverT { runSolverT :: ReaderT (SExpr -> m ()) (ConduitT SExpr Void m) a }
  deriving (Applicative, Functor, Generic, Monad, MonadError e, MonadIO)

instance Monad m => MonadSolver (SolverT m) where
    send msg = SolverT $ do
        sink <- ask
        lift . lift $ sink msg
    recv = SolverT $ do
        fromMaybe (throw NoResponseException) <$> lift await

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
