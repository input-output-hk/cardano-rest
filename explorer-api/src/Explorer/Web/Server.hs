{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Explorer.Web.Server
  ( runServer
  , WebserverConfig(..)
  ) where

import Cardano.Db
    ( Ada, Block (..), PGConfig (..), queryTotalSupply, toConnectionString )
import Cardano.Rest.Types
    ( WebserverConfig (..), toWarpSettings )
import Cardano.Rest.Web as Web
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Logger
    ( logInfoN, runStdoutLoggingT )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, throwE )
import Data.ByteString
    ( ByteString )
import qualified Data.ByteString.Base16 as Base16
import Data.Text
    ( Text )
import Data.Text.Encoding
    ( decodeUtf8 )
import qualified Data.Text.Encoding as Text
import Database.Persist.Postgresql
    ( withPostgresqlConn )
import Database.Persist.Sql
    ( SqlBackend, SqlPersistT )
import Explorer.Web.Api
    ( ExplorerApi, explorerApi )
import Explorer.Web.Api.HttpBridge
    ( HttpBridgeApi (..) )
import Explorer.Web.Api.HttpBridge.AddressBalance
import Explorer.Web.Api.Legacy
    ( ExplorerApiRecord (..) )
import Explorer.Web.Api.Legacy.AddressSummary
import Explorer.Web.Api.Legacy.BlockAddress
import Explorer.Web.Api.Legacy.BlockPagesTotal
import Explorer.Web.Api.Legacy.BlocksPages
import Explorer.Web.Api.Legacy.BlocksTxs
import Explorer.Web.Api.Legacy.EpochPage
import Explorer.Web.Api.Legacy.EpochSlot
import Explorer.Web.Api.Legacy.GenesisAddress
import Explorer.Web.Api.Legacy.GenesisPages
import Explorer.Web.Api.Legacy.GenesisSummary
import Explorer.Web.Api.Legacy.StatsTxs
import Explorer.Web.Api.Legacy.TxLast
import Explorer.Web.Api.Legacy.TxsSummary
import Explorer.Web.Api.Legacy.Util
import Explorer.Web.ClientTypes
    ( CBlockEntry (..), CBlockSummary (..), CHash (..), adaToCCoin )
import Explorer.Web.Error
    ( ExplorerError (..) )
import Explorer.Web.Query
    ( queryBlockSummary )
import Servant
    ( Application, ServerT )
import qualified Servant
import Servant.API
    ( (:<|>) ((:<|>)) )
import Servant.API.Generic
    ( toServant )
import Servant.Server.Generic
    ( AsServerT )

runServer :: WebserverConfig -> PGConfig -> IO ()
runServer webserverConfig pgConfig =
  runStdoutLoggingT $ do
    let pgurl = pgcHost pgConfig <> ":" <> pgcPort pgConfig
    logInfoN $ "Connecting to database at " <> decodeUtf8 pgurl
    withPostgresqlConn (toConnectionString pgConfig) $ \backend ->
      liftIO $ Web.runSettings (toWarpSettings webserverConfig) (explorerApp backend)

explorerApp :: SqlBackend -> Application
explorerApp backend = Servant.serve explorerApi (Servant.hoistServer explorerApi (runQuery backend) explorerHandlers)

explorerHandlers :: forall m. MonadIO m => ServerT ExplorerApi (SqlPersistT m)
explorerHandlers =
    toServant oldHandlers
      :<|> toServant httpBridgeHandlers
  where
    oldHandlers :: ExplorerApiRecord (AsServerT (SqlPersistT m))
    oldHandlers = ExplorerApiRecord
      { _totalAda           = totalAda
      , _blocksPages        = blocksPages
      , _blocksPagesTotal   = blockPagesTotal
      , _blocksSummary      = blocksSummary
      , _blocksTxs          = blocksTxs
      , _txsLast            = getLastTxs
      , _txsSummary         = txsSummary
      , _addressSummary     = addressSummary
      , _epochPages         = epochPage
      , _epochSlots         = epochSlot
      , _genesisSummary     = genesisSummary
      , _genesisPagesTotal  = genesisPages
      , _genesisAddressInfo = genesisAddressInfo
      , _statsTxs           = statsTxs
      , _blockAddress       = blockAddress
      }

    httpBridgeHandlers :: HttpBridgeApi (AsServerT (SqlPersistT m))
    httpBridgeHandlers = HttpBridgeApi
      { _addressBalance     = addressBalance
      }

--------------------------------------------------------------------------------
-- sample data --
--------------------------------------------------------------------------------

totalAda :: MonadIO m => SqlPersistT m (Either ExplorerError Ada)
totalAda = Right <$> queryTotalSupply

hexToBytestring :: Monad m => Text -> ExceptT ExplorerError m ByteString
hexToBytestring text = do
  case Base16.decode (Text.encodeUtf8 text) of
    (blob, "") -> pure blob
    (_partial, remain) -> throwE $ Internal $ "cant parse " <> Text.decodeUtf8 remain <> " as hex"

blocksSummary
    :: MonadIO m
    => CHash
    -> SqlPersistT m (Either ExplorerError CBlockSummary)
blocksSummary (CHash blkHashTxt) = runExceptT $ do
  blkHash <- hexToBytestring blkHashTxt
  liftIO $ print (blkHashTxt, blkHash)
  mBlk <- ExceptT (Right <$> queryBlockSummary blkHash)
  case mBlk of
    Just (blk, prevHash, nextHash, txCount, fees, totalOut, slh, mts) ->
      case blockSlotNo blk of
        Just slotno -> do
          let (epoch, slot) = slotno `divMod` slotsPerEpoch
          pure $ CBlockSummary
            { cbsEntry = CBlockEntry
               { cbeEpoch = epoch
               , cbeSlot = fromIntegral slot
               -- Use '0' for EBBs.
               , cbeBlkHeight = maybe 0 fromIntegral $ blockBlockNo blk
               , cbeBlkHash = CHash . bsBase16Encode $ blockHash blk
               , cbeTimeIssued = mts
               , cbeTxNum = txCount
               , cbeTotalSent = adaToCCoin totalOut
               , cbeSize = blockSize blk
               , cbeBlockLead = Just $ bsBase16Encode slh
               , cbeFees = adaToCCoin fees
               }
            , cbsPrevHash = CHash $ bsBase16Encode prevHash
            , cbsNextHash = fmap (CHash . bsBase16Encode) nextHash
            , cbsMerkleRoot = CHash $ maybe "" bsBase16Encode (blockMerkelRoot blk)
            }
        Nothing -> throwE $ Internal "slot missing"
    _ -> throwE $ Internal "No block found"
