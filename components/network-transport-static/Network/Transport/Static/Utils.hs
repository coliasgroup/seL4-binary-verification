{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Network.Transport.Static.Utils
    ( HandlesForPeerOps (..)
    , peerOpsFromHandles
    , withDriverPeers
    , workerPeers
    ) where

import Network.Transport
import Network.Transport.Static

import Control.Exception.Safe
import Control.Monad (when)
import qualified Data.ByteString as B
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Prelude hiding (foldr)
import System.IO (Handle, hFlush, stdin, stdout)
import System.IO.Error (eofErrorType, mkIOError)
import System.Process (CreateProcess (..), StdStream (CreatePipe),
                       withCreateProcess)

-- TODO test terminating processes

data HandlesForPeerOps
  = HandlesForPeerOps
      { recv :: Handle
      , send :: Handle
      }
  deriving (Eq, Generic)

peerOpsFromHandles :: HandlesForPeerOps -> PeerOps IOException
peerOpsFromHandles handles = PeerOps
    { recv = tryIO $ do
        bs <- B.hGetSome handles.recv 65536
        when (B.null bs) $ do
            ioError $ mkIOError eofErrorType "peer recv handle EOF" (Just handles.recv) Nothing
        return bs
    , send = \bs -> tryIO $ do
        B.hPut handles.send bs
    , flush = tryIO $ do
        hFlush handles.send
    }

workerPeers :: EndPointAddress -> Peers IOException
workerPeers driverAddr = Peers $ M.singleton driverAddr $ peerOpsFromHandles $ HandlesForPeerOps
    { recv = stdin
    , send = stdout
    }

withDriverPeers
    :: M.Map EndPointAddress CreateProcess
    -> (Peers IOException -> Map EndPointAddress Handle -> IO a)
    -> IO a
withDriverPeers workerCmds m =
    go [] (M.toList workerCmds)
  where
    go acc ((addr, cmd):rest) =
        let cmd' = cmd
                { std_in = CreatePipe
                , std_out = CreatePipe
                }
         in withCreateProcess cmd' $ \(Just hin) (Just hout) stderr _ph ->
                let ops = peerOpsFromHandles $ HandlesForPeerOps
                        { recv = hout
                        , send = hin
                        }
                 in go ((addr, ops, stderr):acc) rest
    go acc [] =
        let (peers, stderrs) = flip foldMap acc $ \(addr, ops, stderr) ->
                (M.singleton addr ops, maybe M.empty (M.singleton addr) stderr)
         in m (Peers peers) stderrs
