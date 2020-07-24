{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Explorer.Web.Query
  ( queryBlockHash
  , queryBlockSummary
  , queryChainTip
  , queryNextBlock
  ) where

import Cardano.Db
    ( Ada
    , Block
    , BlockId
    , EntityField (..)
    , blockPrevious
    , blockSlotNo
    , isJust
    , querySelectCount
    , querySlotUtcTime
    , unValueSumAda
    )
import Control.Monad.IO.Class
    ( MonadIO )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( listToMaybe )
import Data.Time.Clock.POSIX
    ( POSIXTime, utcTimeToPOSIXSeconds )
import Data.Word
    ( Word64 )
import Database.Esqueleto
    ( InnerJoin (..)
    , Value (..)
    , desc
    , from
    , in_
    , limit
    , on
    , orderBy
    , select
    , subList_select
    , sum_
    , unValue
    , val
    , where_
    , (==.)
    , (^.)
    )
import Database.Persist.Sql
    ( Entity (..), SqlPersistT )
import Explorer.Web.Api.Legacy.Util
    ( bsBase16Encode )
import Explorer.Web.ClientTypes
    ( CChainTip (..), CHash (..) )

queryBlockHash :: MonadIO m => BlockId -> SqlPersistT m (Maybe ByteString)
queryBlockHash blkid = do
  rows <- select . from $ \blk -> do
    where_ $ blk ^. BlockId ==. val blkid
    pure $ blk ^. BlockHash
  pure (unValue <$> listToMaybe rows)

queryChainTip :: MonadIO m => SqlPersistT m CChainTip
queryChainTip = do
    rows <- select . from $ \ blk -> do
              where_ (isJust (blk ^. BlockBlockNo))
              orderBy [desc (blk ^. BlockBlockNo)]
              limit 1
              pure (blk ^. BlockBlockNo, blk ^. BlockSlotNo, blk ^. BlockHash)
    pure $ maybe defTip convert (listToMaybe rows)
  where
    convert :: (Value (Maybe Word64), Value (Maybe Word64), Value ByteString) -> CChainTip
    convert (Value mBlkNo, Value mSlotNo, Value blkHash) =
      CChainTip
        { ctBlockNo = maybe 0 fromIntegral mBlkNo
        , ctSlotNo = maybe 0 fromIntegral mSlotNo
        , ctBlockHash = CHash (bsBase16Encode blkHash)
        }

    defTip :: CChainTip
    defTip = CChainTip 0 0 (CHash "unknown")

queryNextBlock :: MonadIO m => BlockId -> SqlPersistT m (Maybe ByteString)
queryNextBlock blkid = do
  rows <- select . from $ \blk2 -> do
    where_ $ blk2 ^. BlockPrevious ==. val (Just blkid)
    pure $ blk2 ^. BlockHash
  pure (unValue <$> listToMaybe rows)

queryBlockTxInCount :: MonadIO m => BlockId -> SqlPersistT m Word
queryBlockTxInCount blkid =
  querySelectCount $ \tx -> where_ (tx ^. TxBlock ==. val blkid)

queryBlockByHash :: MonadIO m => ByteString -> SqlPersistT m (Maybe (BlockId, Block, ByteString))
queryBlockByHash blkHash = do
    rows <- select . from $ \ (blk `InnerJoin` sl)-> do
              on (blk ^. BlockSlotLeader ==. sl ^. SlotLeaderId)
              where_ $ blk ^. BlockHash ==. val blkHash
              pure (blk, sl ^. SlotLeaderHash)
    pure $ fmap convert (listToMaybe rows)
  where
    convert :: (Entity Block, Value ByteString) -> (BlockId, Block, ByteString)
    convert (eb, vsh) = (entityKey eb, entityVal eb, unValue vsh)

queryBlockSummary ::
  MonadIO m
  => ByteString -> SqlPersistT m (Maybe (Block, ByteString, Maybe ByteString, Word, Ada, Ada, ByteString, Maybe POSIXTime))
queryBlockSummary blkHash = do
  maybeBlock <- queryBlockByHash blkHash
  case maybeBlock of
    Just (blkid, blk, slh) -> do
      txCount <- queryBlockTxInCount blkid
      fees <- queryTotalFeeInBlock blkid
      totalOut <- queryTotalOutputCoinInBlock blkid
      timestamp <- maybe (pure Nothing) querySlotTimeSeconds $ blockSlotNo blk
      case blockPrevious blk of
        Just prevblkid -> do
          mPrevHash <- queryBlockHash prevblkid
          nextHash <- queryNextBlock blkid
          case mPrevHash of
            Nothing -> pure Nothing
            Just previousHash -> pure $ Just (blk, previousHash, nextHash, txCount, fees, totalOut, slh, timestamp)
        Nothing -> pure Nothing
    Nothing -> pure Nothing

querySlotTimeSeconds :: MonadIO m => Word64 -> SqlPersistT m (Maybe POSIXTime)
querySlotTimeSeconds slotNo =
  either (const Nothing) (Just . utcTimeToPOSIXSeconds) <$> querySlotUtcTime slotNo

queryTotalFeeInBlock :: MonadIO m => BlockId -> SqlPersistT m Ada
queryTotalFeeInBlock blockid = do
  res <- select . from $ \ tx -> do
          where_ (tx ^. TxBlock ==. val blockid)
          pure $ sum_ (tx ^. TxFee)
  pure $ unValueSumAda $ listToMaybe res

queryTotalOutputCoinInBlock :: MonadIO m => BlockId -> SqlPersistT m Ada
queryTotalOutputCoinInBlock blockid = do
    res <- select . from $ \ txOut -> do
            where_ $ txOut ^. TxOutTxId `in_` subQuery
            pure $ sum_ (txOut ^. TxOutValue)
    pure $ unValueSumAda (listToMaybe res)
  where
    subQuery = subList_select . from $ \ tx -> do
        where_ (tx ^. TxBlock ==. val blockid)
        pure $ tx ^. TxId
