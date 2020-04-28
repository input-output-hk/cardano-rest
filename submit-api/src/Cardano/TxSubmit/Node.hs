{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.TxSubmit.Node
  ( ConfigFile (..)
  , TxSubmitNodeParams (..)
  , GenesisFile (..)
  , GenesisHash (..)
  , SocketPath (..)
  , runTxSubmitNode
  ) where

import Cardano.Prelude hiding
    ( Nat, atomically, option, (%) )

import Cardano.Binary
    ( unAnnotated )
import Cardano.BM.Data.Tracer
    ( ToLogObject (..), nullTracer )
import Cardano.BM.Trace
    ( Trace, appendName, logInfo )
import Cardano.TxSubmit.Config
import Cardano.TxSubmit.Metrics
import Cardano.TxSubmit.Tracing.ToObjectOrphans
    ()
import Cardano.TxSubmit.Tx
import Cardano.TxSubmit.Types
import Cardano.TxSubmit.Util
import Control.Monad.IO.Class
    ( liftIO )
import Control.Tracer
    ( Tracer )
import Data.Functor.Contravariant
    ( contramap )
import Data.Text
    ( Text )
import Data.Void
    ( Void )
import Network.Socket
    ( SockAddr (..) )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock (..), GenTx )
import Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion
    ( ByronNodeToClientVersion(..) )
import Ouroboros.Consensus.Cardano
    ( Protocol (..), protocolInfo )
import Ouroboros.Consensus.Config
    ( TopLevelConfig (..) )
import Ouroboros.Consensus.Mempool.API
    ( ApplyTxErr )
import Ouroboros.Consensus.Node.ErrorPolicy
    ( consensusErrorPolicy )
import Ouroboros.Consensus.Node.ProtocolInfo
    ( pInfoConfig )
import Ouroboros.Consensus.Node.Run
    ( nodeNetworkMagic )
import Ouroboros.Consensus.Network.NodeToClient
    (clientCodecs, cChainSyncCodec, cStateQueryCodec, cTxSubmissionCodec)
import Ouroboros.Network.Driver.Simple
    ( runPeer )
import Ouroboros.Network.Mux
    ( AppType (..)
    , MuxPeer (..)
    , OuroborosApplication (..)
    , RunMiniProtocol (..)
    )
import Ouroboros.Network.NodeToClient (ClientSubscriptionParams (..),
                    ConnectionId, ErrorPolicyTrace (..), LocalAddress,
                    NetworkSubscriptionTracers (..), NodeToClientProtocols (..),
                    NodeToClientVersion (..) , NodeToClientVersionData (..),
                    WithAddr (..), ncSubscriptionWorker,
                    networkErrorPolicies, newNetworkMutableState, withIOManager, localSnocket,
                    chainSyncPeerNull, localStateQueryPeerNull, versionedNodeToClientProtocols
    , ErrorPolicyTrace (..)
    , NodeToClientProtocols (..)
    , NodeToClientVersionData (..)
    , WithAddr (..)
    , localSnocket
    , ncSubscriptionWorker
    , networkErrorPolicies
    , newNetworkMutableState
    , withIOManager
    )
import Ouroboros.Consensus.Node.NetworkProtocolVersion (
                    nodeToClientProtocolVersion , supportedNodeToClientVersions)

import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxClientStIdle (..)
    , LocalTxSubmissionClient (..)
    , localTxSubmissionClientPeer
    )
import Ouroboros.Network.Snocket
    ( LocalAddress (..) )
import Ouroboros.Network.Protocol.Handshake.Version
    (DictVersion, Versions, foldMapVersions)

import qualified Cardano.Chain.Genesis as Ledger
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Crypto as Crypto
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text

import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge

data Peer = Peer SockAddr SockAddr deriving Show

-- | The product type of all command line arguments
data TxSubmitNodeParams = TxSubmitNodeParams
  { tspConfigFile :: !ConfigFile
  , tspGenesisFile :: !GenesisFile
  , tspSocketPath :: !SocketPath
  , tspWebPort :: !TxSubmitPort
  }

newtype ConfigFile = ConfigFile
  { unConfigFile :: FilePath
  }

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  }

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  }


runTxSubmitNode :: TxSubmitVar -> Trace IO Text -> Genesis.Config -> SocketPath -> IO ()
runTxSubmitNode tsv trce gc socket = do
  logInfo trce "Running tx-submit node"
  logException trce "tx-submit-node." $ do
    logProtocolMagic trce $ Ledger.configProtocolMagic gc
    void $ runTxSubmitNodeClient tsv (mkNodeConfig gc) trce socket


mkNodeConfig :: Genesis.Config -> TopLevelConfig ByronBlock
mkNodeConfig gc =
  pInfoConfig . protocolInfo $ ProtocolRealPBFT gc Nothing (Update.ProtocolVersion 0 2 0)
      (Update.SoftwareVersion (Update.ApplicationName "cardano-sl") 1) Nothing


runTxSubmitNodeClient
  :: TxSubmitVar -> TopLevelConfig ByronBlock
  -> Trace IO Text -> SocketPath
  -> IO Void
runTxSubmitNodeClient txv topLevelConfig trce (SocketPath socketPath) = do
  logInfo trce $ "localInitiatorNetworkApplication: connecting to node via " <> textShow socketPath
  networkState <- newNetworkMutableState
  withIOManager $ \iocp -> do
    ncSubscriptionWorker
      (localSnocket iocp socketPath)
      -- TODO: these tracers should be configurable for debugging purposes.
      NetworkSubscriptionTracers {
          nsMuxTracer = nullTracer,
          nsHandshakeTracer = nullTracer,
          nsErrorPolicyTracer = errorPolicyTracer,
          nsSubscriptionTracer = nullTracer
          -- TODO subscription tracer should not be 'nullTracer' by default
        }
      networkState
      ClientSubscriptionParams
          { cspAddress = LocalAddress socketPath
          , cspConnectionAttemptDelay = Nothing
          , cspErrorPolicies = networkErrorPolicies <> (consensusErrorPolicy proxyByronBlock)
          }
      txSubmitVersions
  where
    proxyByronBlock :: Proxy ByronBlock
    proxyByronBlock = Proxy

    errorPolicyTracer :: Tracer IO (WithAddr LocalAddress ErrorPolicyTrace)
    errorPolicyTracer = contramap (Text.pack . show). toLogObject $ appendName "ErrorPolicy" trce

    txSubmitVersions
      :: Versions
        NodeToClientVersion
        DictVersion
        (ConnectionId LocalAddress
          -> OuroborosApplication 'InitiatorApp LBS.ByteString IO () Void)
    txSubmitVersions = foldMapVersions
        (\v ->
          versionedNodeToClientProtocols
            (nodeToClientProtocolVersion proxyByronBlock v)
            (NodeToClientVersionData { networkMagic = nodeNetworkMagic proxyByronBlock topLevelConfig })
            (txSubmitProtocols v trce topLevelConfig txv)
        )
        (supportedNodeToClientVersions proxyByronBlock)

txSubmitProtocols
  :: ByronNodeToClientVersion
  -> Trace IO Text
  -> TopLevelConfig ByronBlock
  -> TxSubmitVar
  -> NodeToClientProtocols 'InitiatorApp LBS.ByteString IO () Void
txSubmitProtocols byronVersion trce topLevelConfig txv =
    NodeToClientProtocols {
          localChainSyncProtocol = localChainSyncProtocol
        , localTxSubmissionProtocol = localTxSubmissionProtocol
        , localStateQueryProtocol = dummyLocalQueryProtocol
        }
  where
    codecs = clientCodecs (configBlock topLevelConfig) byronVersion

    dummyLocalQueryProtocol :: RunMiniProtocol 'InitiatorApp LBS.ByteString IO () Void
    dummyLocalQueryProtocol = InitiatorProtocolOnly $ MuxPeer
        nullTracer
        (cStateQueryCodec codecs)
        localStateQueryPeerNull

    localChainSyncProtocol :: RunMiniProtocol 'InitiatorApp LBS.ByteString IO () Void
    localChainSyncProtocol = InitiatorProtocolOnly $ MuxPeer
        nullTracer
        (cChainSyncCodec codecs)
        chainSyncPeerNull

    localTxSubmissionProtocol :: RunMiniProtocol 'InitiatorApp LBS.ByteString IO () Void
    localTxSubmissionProtocol = InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
        logException trce "LocalTxSubmissionPtcl: " $ do
            (metrics, _server) <- registerMetricsServer
            runPeer
                (contramap (Text.pack . show) . toLogObject $ appendName "cardano-tx-submit" trce)
                (cTxSubmissionCodec codecs)
                channel
                (localTxSubmissionClientPeer (txSubmissionClient txv metrics))

-- | A 'LocalTxSubmissionClient' that submits transactions reading them from
-- a 'StrictTMVar'.  A real implementation should use a better synchronisation
-- primitive.  This demo creates and empty 'TMVar' in
-- 'muxLocalInitiatorNetworkApplication' above and never fills it with a tx.
--
txSubmissionClient
  :: TxSubmitVar -> TxSubmitMetrics
  -> LocalTxSubmissionClient (GenTx ByronBlock) (ApplyTxErr ByronBlock) IO ()
txSubmissionClient tsv metrics =
    LocalTxSubmissionClient $
      readTxSubmit tsv >>= pure . loop
  where
    loop :: GenTx ByronBlock -> LocalTxClientStIdle (GenTx ByronBlock) (ApplyTxErr ByronBlock) IO ()
    loop tx =
      SendMsgSubmitTx tx $ \mbreject -> do
        case mbreject of
          Nothing -> liftIO $ Gauge.inc (tsmCount metrics)
          Just _r -> return ()
        writeTxSubmitResponse tsv mbreject
        nextTx <- readTxSubmit tsv
        pure $ loop nextTx

logProtocolMagic :: Trace IO Text -> Crypto.ProtocolMagic -> IO ()
logProtocolMagic tracer pm =
  liftIO . logInfo tracer $ mconcat
    [ "NetworkMagic: ", textShow (Crypto.getRequiresNetworkMagic pm), " "
    , textShow (Crypto.unProtocolMagicId . unAnnotated $ Crypto.getAProtocolMagicId pm)
    ]
