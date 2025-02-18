{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Network.Transport.Static
    ( PeerOps (..)
    , Peers (..)
    , withStaticTransport
    ) where

import Network.Transport.Static.Peers

import Network.Transport

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (handle, throw)
import Control.Exception.Safe
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError),
                             runExcept, runExceptT, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO), askRunInIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.State (MonadState (..), StateT (runStateT), lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BSC (pack)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Optics
import Prelude hiding (foldr)
import System.IO (Handle, hFlush)
import System.IO.Error (eofErrorType, mkIOError)

data TransportState
  = TransportValid ValidTransportState
  | TransportClosed
  deriving (Generic)

data ValidTransportState
  = ValidTransportState
      { selfEndPointAddress :: EndPointAddress
      , peerEndpointAddresses :: Set EndPointAddress
      , peerEventSource :: PeerEventSource'
      , peerMessageSink :: PeerMessageSink'
      , endPointState :: Maybe LocalEndPointState
      }
  deriving (Generic)

data LocalEndPointState
  = LocalEndPointValid ValidLocalEndPointState
  | LocalEndPointClosed
      { eventHasBeenReceived :: Bool
      }
  deriving (Generic)

data ValidLocalEndPointState
  = ValidLocalEndPointState
      { peers :: Map EndPointAddress PeerState
      , nextConnectionId :: ConnectionId
      , connections :: Map (EndPointAddress, ConnectionId) LocalConnection
      }
  deriving (Generic)

newValidLocalEndPointState :: Set EndPointAddress -> ValidLocalEndPointState
newValidLocalEndPointState peerEndpointAddresses = ValidLocalEndPointState
    { peers = M.fromList [ (addr, newPeerState) | addr <- toList peerEndpointAddresses ]
    , nextConnectionId = 0
    , connections = M.empty
    }

data PeerState
  = PeerState
      { maxSharedConnectionId :: SharedConnectionId
      }
  deriving (Generic)

newPeerState :: PeerState
newPeerState = PeerState
    { maxSharedConnectionId = SharedConnectionId 0
    }

data LocalConnection
  = LocalConnection
      { id :: ConnectionId
      , localAddress :: EndPointAddress
      , remoteAddress :: EndPointAddress
      , state :: LocalConnectionState
      }
  deriving (Generic)

data LocalConnectionState
  = LocalConnectionValid
  | LocalConnectionClosed
  | LocalConnectionFailed
  deriving (Generic)

withStaticTransport
    :: (MonadUnliftIO m, MonadLogger m, MonadThrow m, Typeable e, Show e)
    => EndPointAddress
    -> Peers e
    -> (Transport -> m a)
    -> m a
withStaticTransport selfEndPointAddress peersOps m = do
    withPeers' peersOps $ \peerEventSource peerMessageSink -> do
        tsv <- liftIO $ newTVarIO $ TransportValid $ ValidTransportState
            { selfEndPointAddress
            , peerEndpointAddresses = M.keysSet peersOps.unwrap
            , peerEventSource
            , peerMessageSink
            , endPointState = Nothing
            }
        let transport = Transport
                { newEndPoint = apiNewEndpoint tsv
                , closeTransport = apiCloseTransport tsv
                }
        m transport <* liftIO transport.closeTransport

apiCloseTransport :: TVar TransportState -> IO ()
apiCloseTransport tsv = atomically $ withTransportState tsv $ do
    -- TODO close endpoint?
    -- zoomMaybe #_TransportValid $ do
        -- undefined
    put TransportClosed

apiNewEndpoint :: TVar TransportState -> IO (Either (TransportError NewEndPointErrorCode) EndPoint)
apiNewEndpoint tsv = atomically $ withTransportState tsv $ do
    zoomOrThrowString "apiNewEndpoint TransportValid" #_TransportValid $ do
        peerEndpointAddresses <- use #peerEndpointAddresses
        address <- use #selfEndPointAddress
        zoom #endPointState $ do
            get >>= \case
                Just _ -> do
                    throwString "apiNewEndpoint Just"
                Nothing -> do
                    put $ Just $ LocalEndPointValid $ newValidLocalEndPointState peerEndpointAddresses
        return $ Right $ EndPoint
            { receive = apiReceive tsv
            , address
            , connect = apiConnect tsv
            , newMulticastGroup = throwString "newMulticastGroup"
            , resolveMulticastGroup = \_ -> throwString "resolveMulticastGroup"
            , closeEndPoint = apiCloseEndPoint tsv
            }

apiCloseEndPoint :: TVar TransportState -> IO ()
apiCloseEndPoint tsv = atomically $ withTransportState tsv $ do
    zoomOrThrowString "apiCloseEndPoint endPointState" (#_TransportValid % #endPointState) $ do
        get >>= \case
            Nothing -> do
                return ()
            Just _ -> do
                -- TODO close connections with peers?
                return ()
        put $ Just $ LocalEndPointClosed { eventHasBeenReceived = False }

apiReceive :: TVar TransportState -> IO Event
apiReceive tsv = atomically $ withTransportState tsv $ do
    zoomOrThrowString "apiReceive TransportValid" #_TransportValid $ do
        peerEventSource <- use #peerEventSource
        zoomOrThrowString "apiReceive LocalEndPointState" (#endPointState % _Just) $ do
            get >>= \case
                LocalEndPointValid vleps -> do
                    (peerAddr, peerEvent) <- lift peerEventSource
                    case peerEvent of
                        Left ex -> do
                            undefined
                        Right msg -> do
                            case msg of
                                Open scid -> do
                                    undefined
                                Close scid -> do
                                    undefined
                                Send scid bs -> do
                                    undefined
                LocalEndPointClosed { eventHasBeenReceived } -> do
                    if eventHasBeenReceived
                    then return EndPointClosed
                    else throwString "endpoint closed"

apiConnect :: TVar TransportState
           -> EndPointAddress
           -> Reliability
           -> ConnectHints
           -> IO (Either (TransportError ConnectErrorCode) Connection)
apiConnect tsv theirAddress _reliability _hints = do
    let cid = undefined
    return $ Right $ Connection
        { send  = apiSend tsv cid
        , close = apiClose tsv cid
        }

apiSend :: TVar TransportState
        -> ConnectionId
        -> [ByteString]
        -> IO (Either (TransportError SendErrorCode) ())
apiSend tsv cid bs = do
    undefined

apiClose :: TVar TransportState
         -> ConnectionId
         -> IO ()
apiClose tsv cid = do
    undefined

--

withTransportState :: TVar TransportState -> StateT TransportState STM a -> STM a
withTransportState tsv m = do
    ts <- readTVar tsv
    (a, ts') <- runStateT m ts
    writeTVar tsv ts'
    return a

zoomOr :: (Zoom m n s t, Is k An_AffineTraversal) => n c -> Optic' k is t s -> m c -> n c
zoomOr fallback o m = zoomMaybe o m >>= maybe fallback return

zoomOrThrowString :: (Zoom m n s t, MonadThrow n, Is k An_AffineTraversal) => String -> Optic' k is t s -> m c -> n c
zoomOrThrowString err o m = zoomMaybe o m >>= maybe (throwString err) return

-- overValidTransportState :: TVar TransportState -> STM a -> (ValidTransportState -> STM a) -> STM a
-- overValidTransportState tsv fallback action = do
--     ts <- readTVar tsv
--     case ts of
--         TransportValid vts -> action vts
--         _ -> fallback

-- withValidTransportState :: (Typeable e, Show e) => TVar TransportState -> e -> (ValidTransportState -> STM a) -> STM a
-- withValidTransportState ts ex = overValidTransportState ts (throwSTM (TransportError ex "transport closed"))

--

data Bug
  = Bug String
  deriving (Show, Typeable)

instance Exception Bug where
