{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Use join" #-}

module Network.Transport.Static
    ( PeerOps (..)
    , Peers (..)
    , withStaticTransport
    ) where

import Network.Transport.Static.Peers

import Network.Transport

import Control.Applicative (asum)
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar,
                               writeTVar)
import Control.Exception.Safe (Exception, MonadThrow, SomeException,
                               displayException, finally, impureThrow,
                               throwString)
import Control.Monad (join, unless, when)
import Control.Monad.Base (liftBase)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT (runStateT), get, gets, put)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Either (fromRight)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Optics
import Optics.State.Operators ((<<.=))
import Prelude hiding (foldr)

-- import System.IO.Unsafe (unsafePerformIO)

data TransportState
  = TransportValid ValidTransportState
  | TransportClosed
  deriving (Generic)

data ValidTransportState
  = ValidTransportState
      { ctx :: ValidTransportStateContext
      , endPointState :: Maybe LocalEndPointState
      }
  deriving (Generic)

data ValidTransportStateContext
  = ValidTransportStateContext
      { selfEndPointAddress :: EndPointAddress
      , peerEndpointAddresses :: Set EndPointAddress
      , peerEventSource :: PeerEventSource'
      , peerMessageSink :: PeerMessageSink'
      }
  deriving (Generic)

data LocalEndPointState
  = LocalEndPointValid ValidLocalEndPointState
  | LocalEndPointClosed
      { eventHasBeenReceived :: Bool
      }
  deriving (Generic)

newValidLocalEndPointState :: ValidLocalEndPointState
newValidLocalEndPointState = ValidLocalEndPointState
    { connections = M.empty
    , connectionsRev = M.empty
    }

data ValidLocalEndPointState
  = ValidLocalEndPointState
      { connections :: Map ConnectionId LocalConnection
      , connectionsRev :: Map OuterSharedConnectionId ConnectionId
      }
  deriving (Generic)

data OuterSharedConnectionId
  = OuterSharedConnectionId
      { endPointAddress :: EndPointAddress
      , innerId :: SharedConnectionId
      }
  deriving (Eq, Generic, Ord, Show)

data LocalConnection
  = LocalConnection
      { outerId :: OuterSharedConnectionId
      , state :: LocalConnectionState
      }
  deriving (Generic)

data LocalConnectionState
  = LocalConnectionValid
  | LocalConnectionClosed
  | LocalConnectionFailed
  deriving (Eq, Generic, Ord, Show)

--

withStaticTransport
    :: (Typeable e, Show e)
    => EndPointAddress
    -> Peers e
    -> (Transport -> IO a)
    -> IO a
withStaticTransport selfEndPointAddress peersOps m = do
    withPeers' peersOps $ \peerEventSource peerMessageSink -> do
        tsv <- newTVarIO $ TransportValid $ ValidTransportState
            { ctx = ValidTransportStateContext
                { selfEndPointAddress
                , peerEndpointAddresses = M.keysSet peersOps.unwrap
                , peerEventSource
                , peerMessageSink
                }
            , endPointState = Nothing
            }
        let transport = Transport
                { newEndPoint = apiNewEndpoint tsv
                , closeTransport = apiCloseTransport tsv
                }
        m transport `finally` transport.closeTransport

-- TODO idempotence
apiCloseTransport :: TVar TransportState -> IO ()
apiCloseTransport tsv = do
    apiCloseEndPoint tsv
    atomically $ withTransportState tsv $ do
        put TransportClosed

apiNewEndpoint :: TVar TransportState -> IO (Either (TransportError NewEndPointErrorCode) EndPoint)
apiNewEndpoint tsv = atomically $ withValidTransportState tsv $ do
    address <- gview #selfEndPointAddress
    old <- simple <<.= Just (LocalEndPointValid newValidLocalEndPointState)
    when (isJust old) $ do
        throwString "singleton endpoint has already been created"
    let endpoint = EndPoint
            { receive = apiReceive tsv
            , address
            , connect = apiConnect tsv
            , newMulticastGroup = unimplemented "newMulticastGroup"
            , resolveMulticastGroup = \_ -> unimplemented "resolveMulticastGroup"
            , closeEndPoint = apiCloseEndPoint tsv
            }
    return $ Right endpoint

apiCloseEndPoint :: TVar TransportState -> IO ()
apiCloseEndPoint tsv = do
    join $ atomically $ withValidLocalEndpointStateOr (return (return ())) tsv $ do
        conns <- use $ #connections % to M.keys
        return $ for_ conns $ apiClose tsv
    atomically $ withValidTransportStateOr (return ()) tsv $ do
        put $ Just (LocalEndPointClosed
            { eventHasBeenReceived = False
            })

apiReceive :: TVar TransportState -> IO Event
apiReceive tsv = atomically $ withLocalEndpointStateOr fallback tsv $ do
    zoomCasesOrThrow
        [ zoomMaybe #_LocalEndPointValid $ do
            peerEventSource <- gview #peerEventSource
            (peerAddr, peerEvent) <- liftBase peerEventSource
            case peerEvent of
                Left ex -> do
                    return $ ErrorEvent $ transportErrorFromException (EventConnectionLost peerAddr) ex
                Right msg -> do
                    case msg of
                        Open { myConnectionId = theirConnectionId } -> do
                            let outerId = OuterSharedConnectionId peerAddr (TheirConnectionId theirConnectionId)
                            alreadyInUse <- gets $ \vleps -> outerId `M.member` vleps.connectionsRev
                            when alreadyInUse $ do
                                throwString "SharedConnectionId already in use"
                            cid <- nextConnectionId
                            insertNewConnection cid outerId
                            return $ ConnectionOpened cid ReliableOrdered peerAddr
                        Close tscid -> do
                            let outerId = OuterSharedConnectionId peerAddr (receiveSharedConnectionId tscid)
                            zoomConnectionStateByOuterId outerId $ \cid -> do
                                oldSt <- simple <<.= LocalConnectionClosed
                                when (oldSt /= LocalConnectionValid) $ do
                                    throwString "connection was not valid"
                                return $ ConnectionClosed cid
                        Send tscid bs -> do
                            let outerId = OuterSharedConnectionId peerAddr (receiveSharedConnectionId tscid)
                            zoomConnectionStateByOuterId outerId $ \cid -> do
                                st <- get
                                when (st /= LocalConnectionValid) $ do
                                    throwString "connection is not valid"
                                return $ Received cid bs
        , zoomMaybe #_LocalEndPointClosed $ do
            eventHasBeenReceived <- simple <<.= True
            unless eventHasBeenReceived $ do
                throwString "endpoint closed"
            return EndPointClosed
        ]
  where
    -- HACK HACK HACK
    fallback = return EndPointClosed

apiConnect
    :: TVar TransportState
    -> EndPointAddress
    -> Reliability
    -> ConnectHints
    -> IO (Either (TransportError ConnectErrorCode) Connection)
apiConnect tsv peerAddr _reliability _hints = join $ atomically $ withValidLocalEndpointState tsv $ do
    cid <- nextConnectionId
    let outerId = OuterSharedConnectionId peerAddr (OurConnectionId cid)
    insertNewConnection cid outerId
    action <- mkSubmitMessageOr ConnectFailed peerAddr $ Open { myConnectionId = cid }
    return $ runExceptT $ do
        ExceptT action
        return $ Connection
            { send  = apiSend tsv cid
            , close = apiClose tsv cid
            }

apiSend
    :: TVar TransportState
    -> ConnectionId
    -> [ByteString]
    -> IO (Either (TransportError SendErrorCode) ())
apiSend tsv cid bs = join $ atomically $ withValidLocalEndpointState tsv $ do
    zoomConnectionState cid $ \outerId -> do
        st <- get
        case st of
            LocalConnectionValid -> do
                mkSubmitMessageOr SendFailed outerId.endPointAddress $
                    Send (sendSharedConnectionId outerId.innerId) bs
            LocalConnectionClosed -> do
                return $ return $ Left $ TransportError SendClosed ""
            LocalConnectionFailed -> do
                throwString "state == LocalConnectionFailed"

apiClose
    :: TVar TransportState
    -> ConnectionId
    -> IO ()
apiClose tsv cid = join $ atomically $ withValidLocalEndpointState tsv $ do
    zoomConnectionState cid $ \outerId -> do
        oldSt <- simple <<.= LocalConnectionClosed
        case oldSt of
            LocalConnectionValid ->
                mkSubmitMessage_ outerId.endPointAddress $
                    Close (sendSharedConnectionId outerId.innerId)
            _ ->
                return $ return ()

--

withTransportState :: TVar TransportState -> StateT TransportState STM a -> STM a
withTransportState tsv m = do
    ts <- readTVar tsv
    (a, ts') <- runStateT m ts
    writeTVar tsv ts'
    return a

withValidTransportStateOr
    :: STM a
    -> TVar TransportState
    -> ReaderT ValidTransportStateContext (StateT (Maybe LocalEndPointState) STM) a
    -> STM a
withValidTransportStateOr fallback tsv m = withTransportState tsv $ do
    zoomOr (liftBase fallback) #_TransportValid $ do
        ctx <- use #ctx
        runReaderT (zoom #endPointState m) ctx

withValidTransportState
    :: HasCallStack
    => TVar TransportState
    -> ReaderT ValidTransportStateContext (StateT (Maybe LocalEndPointState) STM) a
    -> STM a
withValidTransportState = withValidTransportStateOr $ throwString "invalid transport state"

withLocalEndpointStateOr
    :: STM a
    -> TVar TransportState
    -> ReaderT ValidTransportStateContext (StateT LocalEndPointState STM) a
    -> STM a
withLocalEndpointStateOr fallback tsv =
    withValidTransportStateOr (liftBase fallback) tsv . zoomOr (liftBase fallback) #_Just

withLocalEndpointState
    :: HasCallStack
    => TVar TransportState
    -> ReaderT ValidTransportStateContext (StateT LocalEndPointState STM) a
    -> STM a
withLocalEndpointState = withLocalEndpointStateOr $ throwString "non-existent local endpoint state"

withValidLocalEndpointStateOr
    :: STM a
    -> TVar TransportState
    -> ReaderT ValidTransportStateContext (StateT ValidLocalEndPointState STM) a
    -> STM a
withValidLocalEndpointStateOr fallback tsv =
    withLocalEndpointStateOr (liftBase fallback) tsv . zoomOr (liftBase fallback) #_LocalEndPointValid

withValidLocalEndpointState
    :: HasCallStack
    => TVar TransportState
    -> ReaderT ValidTransportStateContext (StateT ValidLocalEndPointState STM) a
    -> STM a
withValidLocalEndpointState = withValidLocalEndpointStateOr $ throwString "invalid local endpoint state"

nextConnectionId :: MonadState ValidLocalEndPointState m => m ConnectionId
nextConnectionId = do
    maybe 0 ((+ 1) . fst) . M.lookupMax <$> use #connections

zoomConnectionState
    :: ConnectionId
    -> (OuterSharedConnectionId -> ReaderT ValidTransportStateContext (StateT LocalConnectionState STM) a)
    -> ReaderT ValidTransportStateContext (StateT ValidLocalEndPointState STM) a
zoomConnectionState cid m = do
    zoom (#connections % at cid % unwrapped) $ do
        outerId <- use #outerId
        zoom #state $ do
            m outerId

zoomConnectionStateByOuterId
    :: OuterSharedConnectionId
    -> (ConnectionId -> ReaderT ValidTransportStateContext (StateT LocalConnectionState STM) a)
    -> ReaderT ValidTransportStateContext (StateT ValidLocalEndPointState STM) a
zoomConnectionStateByOuterId outerId m = do
    cid <- use $ #connectionsRev % at outerId % unwrapped
    zoomConnectionState cid $ \_ -> do
        m cid

insertNewConnection :: MonadState ValidLocalEndPointState m => ConnectionId -> OuterSharedConnectionId -> m ()
insertNewConnection cid outerId = do
    assign (#connections % at cid) $ Just $ LocalConnection
        { outerId
        , state = LocalConnectionValid
        }
    assign (#connectionsRev % at outerId) $ Just cid

transportErrorFromException :: (Exception e, Typeable a, Show a) => a -> e -> TransportError a
transportErrorFromException code ex =
    -- TransportError code (displayException ex)
    -- HACK
    impureThrow (TransportError code (displayException ex))

mkSubmitMessage :: MonadReader ValidTransportStateContext m => EndPointAddress -> Message -> m (IO (Either SomeException ()))
mkSubmitMessage peerAddr msg = do
    peerMessageSink <- gview #peerMessageSink
    return $ peerMessageSink peerAddr msg

mkSubmitMessageOr :: (MonadReader ValidTransportStateContext m, Typeable a, Show a) => a -> EndPointAddress -> Message -> m (IO (Either (TransportError a) ()))
mkSubmitMessageOr code peerAddr msg = fmap (first (transportErrorFromException code)) <$> mkSubmitMessage peerAddr msg

mkSubmitMessage_ :: MonadReader ValidTransportStateContext m => EndPointAddress -> Message -> m (IO ())
mkSubmitMessage_ peerAddr msg = void <$> mkSubmitMessage peerAddr msg

--

zoomOr :: (Zoom m n s t, Is k An_AffineTraversal) => n c -> Optic' k is t s -> m c -> n c
zoomOr fallback o m = zoomMaybe o m >>= maybe fallback return

zoomOrThrow :: HasCallStack => (Zoom m n s t, MonadThrow n, Is k An_AffineTraversal) => String -> Optic' k is t s -> m c -> n c
zoomOrThrow err = zoomOr (throwString err)

zoomCasesOr :: Monad m => m a -> [m (Maybe a)] -> m a
zoomCasesOr fallback cases = runMaybeT (asum (map MaybeT cases)) >>= maybe fallback return

zoomCasesOrThrow :: HasCallStack => (Monad m, MonadThrow m) => [m (Maybe a)] -> m a
zoomCasesOrThrow = zoomCasesOr (throwString "non-exhaustive zoomC cases")

unwrapped :: HasCallStack => Lens (Maybe a) (Maybe b) a b
unwrapped = expecting _Just

expecting :: HasCallStack => Is k An_AffineTraversal => Optic k is s t a b -> Lens s t a b
expecting optic = withAffineTraversal optic $ \match -> lens (fromRight (error "!isRight") . match)

unimplemented :: MonadThrow m => String -> m a
unimplemented s = throwString $ "unimplemented: " ++ s
