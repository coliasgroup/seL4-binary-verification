module Network.Transport.Static.Peers
    ( Message (..)
    , PeerEventSource
    , PeerEventSource'
    , PeerMessageSink
    , PeerMessageSink'
    , PeerOps (..)
    , Peers (..)
    , SharedConnectionId (..)
    , upcastPeerEventSource
    , upcastPeerMessageSink
    , withPeers
    , withPeers'
    ) where

import Network.Transport

import Control.Concurrent.Async
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, putTMVar, takeTMVar)
import Control.Exception.Safe
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError),
                             runExceptT, withExceptT)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.STM (STM)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import Data.Binary (Binary, Get, encode, get)
import Data.Binary.Get (Decoder (..), pushChunk, runGetIncremental)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Void (absurd)
import GHC.Generics (Generic)

newtype Peers e
  = Peers { unwrap :: Map EndPointAddress (PeerOps e) }
  deriving (Generic)

type PeerEventSource e = STM (EndPointAddress, Either e Message)
type PeerMessageSink e = EndPointAddress -> Message -> IO (Either e ())

type PeerEventSource' = PeerEventSource SomeException
type PeerMessageSink' = PeerMessageSink SomeException

upcastPeerEventSource :: Exception e => PeerEventSource e -> PeerEventSource'
upcastPeerEventSource = fmap (fmap (first toException))

upcastPeerMessageSink :: Exception e => PeerMessageSink e -> PeerMessageSink'
upcastPeerMessageSink = fmap (fmap (fmap (first toException)))

withPeers'
    :: (MonadUnliftIO m, MonadLogger m, MonadThrow m, Typeable e, Show e)
    => Peers e
    -> (PeerEventSource' -> PeerMessageSink' -> m a)
    -> m a
withPeers' peersOps m = withPeers peersOps $ \peerEventSource peerMessageSink ->
    m (upcastPeerEventSource peerEventSource) (upcastPeerMessageSink peerMessageSink)

withPeers
    :: (MonadUnliftIO m, MonadLogger m, MonadThrow m)
    => Peers e
    -> (PeerEventSource (PeerException e) -> PeerMessageSink (PeerException e) -> m a)
    -> m a
withPeers peersOps m = withRunInIO $ \run -> do
    let peerMessageSink addr = runExceptT . sendMessage (peersOps.unwrap ! addr)
    peerEventChan <- newEmptyTMVarIO
    let peerEventSource = takeTMVar peerEventChan
    let forwardEvents = do
            forConcurrently_ (M.toList peersOps.unwrap) $ \(addr, ops) -> do
                let sink = atomically . putTMVar peerEventChan . (addr,)
                recvMessages ops (sink . Right) >>= sink . Left
    withAsync forwardEvents $ \_ ->
        run $ m peerEventSource peerMessageSink

sendMessage :: PeerOps e -> Message -> ExceptT (PeerException e) IO ()
sendMessage ops msg = withExceptT PeerExceptionIO $ do
    mapM_ (ExceptT . ops.send) (BL.toChunks (encode msg))
    ExceptT ops.flush

recvMessages :: PeerOps e -> (Message -> IO ()) -> IO (PeerException e)
recvMessages ops f = either id absurd <$>
    runExceptT (recvGet get (withExceptT PeerExceptionIO (ExceptT ops.recv)) (lift . f))

recvGet :: MonadError (PeerException e) m => Get a -> m ByteString -> (a -> m ()) -> m b
recvGet g recvChunk f = start
  where
    start = do
        chunk <- recvChunk
        go (runGetIncremental g `pushChunk` chunk)
    go = \case
        Done rest _ v -> do
            f v
            if B.null rest
                then start
                else go (runGetIncremental g `pushChunk` rest)
        Partial n -> recvChunk >>= (go . n . Just)
        Fail _ _ err -> throwError (PeerExceptionMalformedMessage err)

data PeerException e
  = PeerExceptionIO e
  | PeerExceptionMalformedMessage String
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance (Typeable e, Show e) => Exception (PeerException e) where

data Message
  = Open SharedConnectionId
  | Close SharedConnectionId
  | Send SharedConnectionId ByteString
  deriving (Eq, Generic, Show)

instance Binary Message

newtype SharedConnectionId
  = SharedConnectionId { unwrap :: Integer }
  deriving (Eq, Generic, Show)

instance Binary SharedConnectionId

data PeerOps e
  = PeerOps
      { recv :: IO (Either e ByteString)
      , send :: ByteString -> IO (Either e ())
      , flush :: IO (Either e ())
      }
  deriving (Generic)
