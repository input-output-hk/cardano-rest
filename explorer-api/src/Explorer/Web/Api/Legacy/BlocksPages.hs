{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Explorer.Web.Api.Legacy.BlocksPages
  ( blocksPages
  ) where

import Cardano.Db
    ( Block (..)
    , DbLovelace (..)
    , EntityField (..)
    , isJust
    , queryBlockHeight
    , unValue2
    )
import Control.Monad.IO.Class
    ( MonadIO )
import Data.ByteString.Char8
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
import Data.Time.Clock.POSIX
    ( utcTimeToPOSIXSeconds )
import Data.Word
    ( Word64 )
import Database.Esqueleto
    ( Entity (..)
    , InnerJoin (..)
    , Value (..)
    , asc
    , desc
    , from
    , limit
    , offset
    , on
    , orderBy
    , select
    , val
    , where_
    , (==.)
    , (^.)
    )
import Database.Persist.Sql
    ( SqlPersistT )
import Explorer.Web.Api.Legacy
    ( PageNumber )
import Explorer.Web.Api.Legacy.Types
    ( PageNo (..), PageSize (..) )
import Explorer.Web.Api.Legacy.Util
    ( bsBase16Encode )
import Explorer.Web.ClientTypes
    ( CBlockEntry (..), CHash (..) )
import Explorer.Web.Error
    ( ExplorerError (..) )

import qualified Data.List as List

-- Example queries:
--
--  /api/blocks/pages
--  /api/blocks/pages?page=0
--  /api/blocks/pages?page=1

blocksPages
    :: MonadIO m
    => Maybe PageNo -> Maybe PageSize
    -> SqlPersistT m (Either ExplorerError (PageNumber, [CBlockEntry]))
blocksPages mPageNo _mPageSize =
      case mPageNo of
        Nothing -> queryLatestBlocksPage
        Just (PageNo 0) -> pure $ Left (Internal "Page number must be greater than 0")
        Just pn -> queryBlocksPageNo pn

queryLatestBlocksPage
    :: MonadIO m
    => SqlPersistT m (Either ExplorerError (PageNumber, [CBlockEntry]))
queryLatestBlocksPage = do
  res <- select . from $ \ (blk `InnerJoin` sl) -> do
          on (blk ^. BlockSlotLeaderId ==. sl ^. SlotLeaderId)
          where_ (isJust $ blk ^. BlockSlotNo)
          orderBy [desc (blk ^. BlockSlotNo)]
          limit 10
          pure (blk, sl ^. SlotLeaderHash)
  case res of
    [] -> pure $ Left (Internal "Number of pages exceeds total page count.")
    _ -> Right <$> createCBlockEntry Nothing res

queryBlocksPageNo
    :: MonadIO m
    => PageNo
    -> SqlPersistT m (Either ExplorerError (PageNumber, [CBlockEntry]))
queryBlocksPageNo (PageNo page) = do
  res <- select . from $ \ (blk `InnerJoin` sl) -> do
          on (blk ^. BlockSlotLeaderId ==. sl ^. SlotLeaderId)
          where_ (isJust $ blk ^. BlockSlotNo)
          orderBy [asc (blk ^. BlockSlotNo)]
          offset (fromIntegral $ (page - 1) * 10)
          limit 10
          pure (blk, sl ^. SlotLeaderHash)
  case res of
    [] -> pure $ Left (Internal "Number of pages exceeds total page count.")
    _ -> Right <$> createCBlockEntry (Just page) (List.reverse res)

createCBlockEntry
    :: MonadIO m
    => Maybe Word -> [(Entity Block, Value ByteString)]
    -> SqlPersistT m (PageNumber, [CBlockEntry])
createCBlockEntry mPageNo xs = do
    blockHeight <- queryBlockHeight
    let pageEntries = maybe (calculatePageEntries blockHeight) (const 10) mPageNo
    ys <- mapM queryCBlockEntry $ List.take pageEntries xs
    pure (toPageNo blockHeight, ys)
  where
    toPageNo :: Word64 -> PageNumber
    toPageNo x =
      case fromIntegral x `divMod` 10 of
        (y, 0) -> y
        (y, _) -> y + 1

    calculatePageEntries :: Word64 -> Int
    calculatePageEntries blockHeight =
      case blockHeight `mod` 10 of
        0 -> 10
        y -> fromIntegral y

queryCBlockEntry
    :: MonadIO m
    => (Entity Block, Value ByteString)
    -> SqlPersistT m CBlockEntry
queryCBlockEntry (Entity blkId block, Value slHash) = do
    rows <- select . from $ \ (blk `InnerJoin` tx) -> do
              on (blk ^. BlockId ==. tx ^. TxBlockId)
              where_ (blk ^. BlockId ==. val blkId)
              pure (tx ^. TxOutSum, tx ^. TxFee)
    pure $ mkCBlockEntry (map unValue2 rows)
  where
    mkCBlockEntry :: [(DbLovelace, DbLovelace)] -> CBlockEntry
    mkCBlockEntry xs =
      CBlockEntry
        { cbeEpoch = fromMaybe 0 (blockEpochNo block)
        , cbeSlot = fromMaybe 0 (blockEpochSlotNo block)
        , cbeBlkHeight = maybe 0 fromIntegral $ blockBlockNo block
        , cbeBlkHash = CHash $ bsBase16Encode (blockHash block)
        , cbeTimeIssued = Just $ utcTimeToPOSIXSeconds (blockTime block)
        , cbeTxNum = fromIntegral $ length xs
        , cbeTotalSent = fromIntegral (sum $ map (unDbLovelace . fst) xs)
        , cbeSize = blockSize block
        , cbeBlockLead = Just $ bsBase16Encode slHash
        , cbeFees = fromIntegral (sum $ map (unDbLovelace . snd) xs)
        }
