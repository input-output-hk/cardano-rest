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

import Cardano.Api.Protocol
    ( Protocol (..)
    , withlocalNodeConnectInfo
    )
import Cardano.Api.Typed
    ( NodeConsensusMode (..)
    , LocalNodeClientProtocols (..)
    , LocalNodeConnectInfo (..)
    , LocalTxSubmissionClient (..)
    , NetworkId (..)
    , connectToLocalNode
    , nullLocalNodeClientProtocols
    )
import Cardano.BM.Trace
    ( Trace, logInfo )
import Cardano.TxSubmit.Config
import Cardano.TxSubmit.Metrics
import Cardano.TxSubmit.Tracing.ToObjectOrphans
    ()
import Cardano.TxSubmit.Tx
import Cardano.TxSubmit.Types
import Cardano.TxSubmit.Util
import Control.Monad.IO.Class
    ( liftIO )
import Data.Text
    ( Text )
import Network.Socket
    ( SockAddr (..) )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock (..), GenTx )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( ApplyTxErr )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxClientStIdle (..)
    , SubmitResult (..)
    )

import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge

data Peer = Peer SockAddr SockAddr deriving Show

-- | The product type of all command line arguments
data TxSubmitNodeParams = TxSubmitNodeParams
  { tspConfigFile :: !ConfigFile
  , tspProtocol :: !Protocol
  , tspNetworkId :: !NetworkId
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


runTxSubmitNode
  :: TxSubmitVar
  -> Trace IO Text
  -> Protocol
  -> NetworkId
  -> SocketPath
  -> IO ()
runTxSubmitNode tsv trce protocol nwId socket = do
  logInfo trce "Running tx-submit node"
  logException trce "tx-submit-node." $
    runTxSubmitNodeClient tsv trce protocol nwId socket


runTxSubmitNodeClient
  :: forall blk. (blk ~ ByronBlock)
  => TxSubmitVar
  -> Trace IO Text
  -> Protocol
  -> NetworkId
  -> SocketPath
  -> IO ()
runTxSubmitNodeClient tsv trce protocol nwId (SocketPath socketPath) = do
    logInfo trce $ "runTxSubmitNodeClient: connecting to node via " <> textShow socketPath
    withlocalNodeConnectInfo protocol nwId socketPath $ \connectInfo ->
      case localNodeConsensusMode connectInfo of
        ByronMode{} -> do
          (metrics, server) <- registerMetricsServer
          connectToLocalNode connectInfo (localNodeClientProtocols metrics)
          cancel server
        _ -> panic "runTxSubmitNodeClient: Currently, only the Byron mode is supported."
  where
    localNodeClientProtocols :: TxSubmitMetrics -> LocalNodeClientProtocols blk
    localNodeClientProtocols metrics =
      nullLocalNodeClientProtocols
        { localTxSubmissionClient = Just (txSubmissionClient tsv metrics)
        }


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
    loop
      :: GenTx ByronBlock
      -> LocalTxClientStIdle (GenTx ByronBlock) (ApplyTxErr ByronBlock) IO ()
    loop tx =
      SendMsgSubmitTx tx $ \submitRes -> do
        case submitRes of
          SubmitSuccess -> liftIO $ Gauge.inc (tsmCount metrics)
          SubmitFail _r -> return ()
        writeTxSubmitResponse tsv submitRes
        nextTx <- readTxSubmit tsv
        pure $ loop nextTx
