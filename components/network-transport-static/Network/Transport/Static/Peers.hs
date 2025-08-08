module Network.Transport.Static.Peers
    ( Message (..)
    , PeerEventSource
    , PeerEventSource'
    , PeerMessageSink
    , PeerMessageSink'
    , PeerOps (..)
    , Peers (..)
    , SharedConnectionId (..)
    , TransitSharedConnectionId (..)
    , receiveSharedConnectionId
    , sendSharedConnectionId
    , upcastPeerEventSource
    , upcastPeerMessageSink
    , withPeers
    , withPeers'
    ) where

import Network.Transport

import Control.Concurrent.Async (forConcurrently_, link, withAsync)
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, putTMVar, takeTMVar)
import Control.Exception.Safe (Exception (toException), SomeException)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError),
                             runExceptT, withExceptT)
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
import Data.Typeable (Typeable)
import Data.Void (absurd)
import GHC.Generics (Generic)

-- TODO keep track of connection errors, so that both send and recv will fail if the other has already failed

newtype Peers e
  = Peers { unwrap :: Map EndPointAddress (PeerOps e) }
  deriving (Generic)

data PeerOps e
  = PeerOps
      { recv :: IO (Either e ByteString)
      , send :: ByteString -> IO (Either e ())
      , flush :: IO (Either e ())
      }
  deriving (Generic)

data PeerException e
  = PeerExceptionOps e
  | PeerExceptionMalformedMessage String
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance (Typeable e, Show e) => Exception (PeerException e)

withPeers'
    :: (Typeable e, Show e)
    => Peers e
    -> (PeerEventSource' -> PeerMessageSink' -> IO a)
    -> IO a
withPeers' peersOps m = withPeers peersOps $ \peerEventSource peerMessageSink ->
    m (upcastPeerEventSource peerEventSource) (upcastPeerMessageSink peerMessageSink)

withPeers
    :: Peers e
    -> (PeerEventSource (PeerException e) -> PeerMessageSink (PeerException e) -> IO a)
    -> IO a
withPeers peersOps m = do
    peerEventChan <- newEmptyTMVarIO
    let peerEventSource = takeTMVar peerEventChan
    let forwardEvents = do
            forConcurrently_ (M.toList peersOps.unwrap) $ \(peerAddr, peerOps) -> do
                let sink = atomically . putTMVar peerEventChan . (peerAddr,)
                recvMessages peerOps (sink . Right) >>= sink . Left
    withAsync forwardEvents $ \a -> do
        link a -- TODO better way to monitor background activity?
        m peerEventSource peerMessageSink
  where
    peerMessageSink addr = runExceptT . sendMessage (peersOps.unwrap ! addr)

data Message
  = Open
      { myConnectionId :: ConnectionId
      }
  | Close TransitSharedConnectionId
  | Send TransitSharedConnectionId [ByteString]
  deriving (Eq, Generic, Show)

instance Binary Message

data TransitSharedConnectionId
  = MyConnectionId ConnectionId
  | YourConnectionId ConnectionId
  deriving (Eq, Generic, Ord, Show)

instance Binary TransitSharedConnectionId

data SharedConnectionId
  = OurConnectionId ConnectionId
  | TheirConnectionId ConnectionId
  deriving (Eq, Generic, Ord, Show)

sendSharedConnectionId :: SharedConnectionId -> TransitSharedConnectionId
sendSharedConnectionId = \case
    OurConnectionId cid -> MyConnectionId cid
    TheirConnectionId cid -> YourConnectionId cid

receiveSharedConnectionId :: TransitSharedConnectionId -> SharedConnectionId
receiveSharedConnectionId = \case
    MyConnectionId cid -> TheirConnectionId cid
    YourConnectionId cid -> OurConnectionId cid

type PeerEventSource e = STM (EndPointAddress, Either e Message)
type PeerMessageSink e = EndPointAddress -> Message -> IO (Either e ())

type PeerEventSource' = PeerEventSource SomeException
type PeerMessageSink' = PeerMessageSink SomeException

upcastPeerEventSource :: Exception e => PeerEventSource e -> PeerEventSource'
upcastPeerEventSource = fmap (fmap (first toException))

upcastPeerMessageSink :: Exception e => PeerMessageSink e -> PeerMessageSink'
upcastPeerMessageSink = fmap (fmap (fmap (first toException)))

sendMessage :: PeerOps e -> Message -> ExceptT (PeerException e) IO ()
sendMessage ops msg = withExceptT PeerExceptionOps $ do
    mapM_ (ExceptT . ops.send) (BL.toChunks (encode msg))
    ExceptT ops.flush

recvMessages :: PeerOps e -> (Message -> IO ()) -> IO (PeerException e)
recvMessages ops f = either id absurd <$>
    runExceptT (recvGet get (withExceptT PeerExceptionOps (ExceptT ops.recv)) (lift . f))

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
