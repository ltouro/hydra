-- | Ouroboros-based implementation of 'Hydra.Network' interface
module Hydra.Network.Ouroboros (
  withOuroborosNetwork,
  TraceOuroborosNetwork,
  module Hydra.Network,
) where

import Hydra.Prelude

import Cardano.Binary (FromCBOR, ToCBOR)
import qualified Codec.CBOR.Term as CBOR
import Control.Concurrent.STM (
  TChan,
  dupTChan,
  newBroadcastTChanIO,
  newTBQueueIO,
  readTBQueue,
  readTChan,
  writeTBQueue,
  writeTChan,
 )
import Control.Monad.Class.MonadAsync (wait)
import Hydra.Logging (Tracer)
import Hydra.Network (
  Host (..),
  Network (..),
  NetworkCallback,
  PortNumber,
 )
import Hydra.Network.Ouroboros.Client as FireForget (
  FireForgetClient (..),
  fireForgetClientPeer,
 )
import Hydra.Network.Ouroboros.Server as FireForget (
  FireForgetServer (..),
  fireForgetServerPeer,
 )
import Hydra.Network.Ouroboros.Type (
  FireForget,
  codecFireForget,
 )
import Network.Mux.Compat (
  MuxTrace,
  WithMuxBearer,
 )
import Network.Socket (
  AddrInfo (addrAddress),
  SockAddr,
  defaultHints,
  getAddrInfo,
 )
import Network.TypedProtocol.Pipelined ()
import Ouroboros.Network.Driver.Simple (
  TraceSendRecv,
 )
import Ouroboros.Network.ErrorPolicy (
  ErrorPolicyTrace,
  WithAddr,
  nullErrorPolicies,
 )
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Mux (
  MiniProtocol (
    MiniProtocol,
    miniProtocolLimits,
    miniProtocolNum,
    miniProtocolRun
  ),
  MiniProtocolLimits (..),
  MiniProtocolNum (MiniProtocolNum),
  MuxMode (..),
  MuxPeer (MuxPeer),
  OuroborosApplication (..),
  RunMiniProtocol (..),
 )
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec, noTimeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import Ouroboros.Network.Protocol.Handshake.Unversioned (
  UnversionedProtocol,
  unversionedHandshakeCodec,
  unversionedProtocol,
  unversionedProtocolDataCodec,
 )
import Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion)
import Ouroboros.Network.Server.Socket (AcceptedConnectionsLimit (AcceptedConnectionsLimit))
import Ouroboros.Network.Snocket (socketSnocket)
import Ouroboros.Network.Socket (
  AcceptConnectionsPolicyTrace,
  ConnectionId (..),
  NetworkConnectTracers (..),
  NetworkServerTracers (..),
  SomeResponderApplication (..),
  connectToNodeSocket,
  newNetworkMutableState,
  withServerNode,
 )
import Ouroboros.Network.Subscription (
  IPSubscriptionTarget (IPSubscriptionTarget),
  SubscriptionTrace,
  WithIPList,
 )
import qualified Ouroboros.Network.Subscription as Subscription
import Ouroboros.Network.Subscription.Ip (SubscriptionParams (..))
import Ouroboros.Network.Subscription.Worker (LocalAddresses (LocalAddresses))

withOuroborosNetwork ::
  forall msg.
  (ToCBOR msg, FromCBOR msg) =>
  Tracer IO (WithHost (TraceOuroborosNetwork msg)) ->
  Host ->
  [Host] ->
  NetworkCallback msg IO ->
  (Network IO msg -> IO ()) ->
  IO ()
withOuroborosNetwork tracer localHost remoteHosts networkCallback between = do
  bchan <- newBroadcastTChanIO
  chanPool <- newTBQueueIO (fromIntegral $ length remoteHosts)
  replicateM_ (length remoteHosts) $
    atomically $ do
      dup <- dupTChan bchan
      writeTBQueue chanPool dup
  withIOManager $ \iomgr -> do
    race_ (connect iomgr chanPool hydraClient) $
      race_ (listen iomgr hydraServer) $ do
        between $
          Network
            { broadcast = atomically . writeTChan bchan
            }
 where
  resolveSockAddr Host{hostName, portNumber} = do
    is <- getAddrInfo (Just defaultHints) (Just $ toString hostName) (Just $ show portNumber)
    case is of
      (info : _) -> pure $ addrAddress info
      _ -> error "getAdrrInfo failed.. do proper error handling"

  connect iomgr chanPool app = do
    -- REVIEW(SN): move outside to have this information available?
    networkState <- newNetworkMutableState
    -- Using port number 0 to let the operating system pick a random port
    localAddr <- resolveSockAddr localHost{portNumber = 0}
    remoteAddrs <- forM remoteHosts resolveSockAddr
    let sn = socketSnocket iomgr
    Subscription.ipSubscriptionWorker
      sn
      (contramap (WithHost localHost . TraceSubscriptions) tracer)
      (contramap (WithHost localHost . TraceErrorPolicy) tracer)
      networkState
      (subscriptionParams localAddr remoteAddrs)
      (actualConnect iomgr chanPool app)

  subscriptionParams localAddr remoteAddrs =
    SubscriptionParams
      { spLocalAddresses = LocalAddresses (Just localAddr) Nothing Nothing
      , spConnectionAttemptDelay = const Nothing
      , spErrorPolicies = nullErrorPolicies
      , spSubscriptionTarget = IPSubscriptionTarget remoteAddrs 7
      }

  actualConnect iomgr chanPool app sn = do
    chan <- atomically $ readTBQueue chanPool
    connectToNodeSocket
      iomgr
      unversionedHandshakeCodec
      noTimeLimitsHandshake
      (cborTermVersionDataCodec unversionedProtocolDataCodec)
      networkConnectTracers
      acceptableVersion
      (unversionedProtocol (app chan))
      sn
   where
    networkConnectTracers =
      NetworkConnectTracers
        { nctMuxTracer = contramap (WithHost localHost . TraceMux) tracer
        , nctHandshakeTracer = contramap (WithHost localHost . TraceHandshake) tracer
        }

  listen iomgr app = do
    networkState <- newNetworkMutableState
    localAddr <- resolveSockAddr localHost
    -- TODO(SN): whats this? _ <- async $ cleanNetworkMutableState networkState
    withServerNode
      (socketSnocket iomgr)
      networkServerTracers
      networkState
      (AcceptedConnectionsLimit maxBound maxBound 0)
      localAddr
      unversionedHandshakeCodec
      noTimeLimitsHandshake
      (cborTermVersionDataCodec unversionedProtocolDataCodec)
      acceptableVersion
      (unversionedProtocol (SomeResponderApplication app))
      nullErrorPolicies
      $ \_ serverAsync -> wait serverAsync -- block until async exception
   where
    networkServerTracers =
      NetworkServerTracers
        { nstMuxTracer = contramap (WithHost localHost . TraceMux) tracer
        , nstHandshakeTracer = contramap (WithHost localHost . TraceHandshake) tracer
        , nstErrorPolicyTracer = contramap (WithHost localHost . TraceErrorPolicy) tracer
        , nstAcceptPolicyTracer = contramap (WithHost localHost . TraceAcceptPolicy) tracer
        }

  hydraClient ::
    TChan msg ->
    OuroborosApplication 'InitiatorMode addr LByteString IO () Void
  hydraClient chan =
    OuroborosApplication $ \_connectionId _controlMessageSTM ->
      [ MiniProtocol
          { miniProtocolNum = MiniProtocolNum 42
          , miniProtocolLimits = maximumMiniProtocolLimits
          , miniProtocolRun = InitiatorProtocolOnly initiator
          }
      ]
   where
    initiator =
      MuxPeer
        (contramap (WithHost localHost . TraceSendRecv) tracer)
        codecFireForget
        (fireForgetClientPeer $ client chan)

  hydraServer ::
    OuroborosApplication 'ResponderMode addr LByteString IO Void ()
  hydraServer =
    OuroborosApplication $ \_connectionId _controlMessageSTM ->
      [ MiniProtocol
          { miniProtocolNum = MiniProtocolNum 42
          , miniProtocolLimits = maximumMiniProtocolLimits
          , miniProtocolRun = ResponderProtocolOnly responder
          }
      ]
   where
    responder =
      MuxPeer
        (contramap (WithHost localHost . TraceSendRecv) tracer)
        codecFireForget
        (fireForgetServerPeer server)

  -- TODO: provide sensible limits
  -- https://github.com/input-output-hk/ouroboros-network/issues/575
  maximumMiniProtocolLimits :: MiniProtocolLimits
  maximumMiniProtocolLimits =
    MiniProtocolLimits{maximumIngressQueue = maxBound}

  client ::
    TChan msg ->
    FireForgetClient msg IO ()
  client chan =
    Idle $ do
      atomically (readTChan chan) <&> \msg ->
        SendMsg msg (pure $ client chan)

  server :: FireForgetServer msg IO ()
  server =
    FireForgetServer
      { recvMsg = \msg -> networkCallback msg $> server
      , recvMsgDone = pure ()
      }

data WithHost trace = WithHost Host trace
  deriving (Show)

data TraceOuroborosNetwork msg
  = TraceSubscriptions (WithIPList (SubscriptionTrace SockAddr))
  | TraceErrorPolicy (WithAddr SockAddr ErrorPolicyTrace)
  | TraceAcceptPolicy AcceptConnectionsPolicyTrace
  | TraceHandshake (WithMuxBearer (ConnectionId SockAddr) (TraceSendRecv (Handshake UnversionedProtocol CBOR.Term)))
  | TraceMux (WithMuxBearer (ConnectionId SockAddr) MuxTrace)
  | TraceSendRecv (TraceSendRecv (FireForget msg))
  deriving (Show)
