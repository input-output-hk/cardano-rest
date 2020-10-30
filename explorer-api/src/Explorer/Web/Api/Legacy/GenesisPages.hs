module Explorer.Web.Api.Legacy.GenesisPages
  ( genesisPages
  ) where

import Cardano.Db
    ( EntityField (..), txOutSpentP, txOutUnspentP )
import Control.Monad.IO.Class
    ( MonadIO )
import Data.Maybe
    ( listToMaybe )
import Database.Esqueleto
    ( InnerJoin (..)
    , Value
    , countRows
    , from
    , on
    , select
    , unValue
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
    ( PageSize (..) )
import Explorer.Web.Api.Legacy.Util
    ( divRoundUp, toPageSize )
import Explorer.Web.ClientTypes
    ( CAddressesFilter (..) )
import Explorer.Web.Error
    ( ExplorerError (..) )


genesisPages
    :: MonadIO m
    => Maybe PageSize
    -> Maybe CAddressesFilter
    -> SqlPersistT m (Either ExplorerError PageNumber)
genesisPages mPageSize mAddrFilter =
      case mAddrFilter of
        Just RedeemedAddresses -> Right <$> queryRedeemedGenesisAddressCount pageSize
        Just NonRedeemedAddresses -> Right <$> queryUnRedeemedGenesisAddressCount pageSize
        _ -> Right <$> queryGenesisAddressCount pageSize
  where
    pageSize = toPageSize mPageSize

queryGenesisAddressCount :: MonadIO m => PageSize -> SqlPersistT m Word
queryGenesisAddressCount (PageSize pageSize) = do
  res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            on (blk ^. BlockId ==. tx ^. TxBlockId)
            -- Only the initial genesis block has a size of 0.
            where_ (blk ^. BlockSize ==. val 0)
            pure countRows
  pure $ maybe 0 (dividePageSize pageSize) (listToMaybe res)

queryRedeemedGenesisAddressCount :: MonadIO m => PageSize -> SqlPersistT m Word
queryRedeemedGenesisAddressCount (PageSize pageSize) = do
  res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            on (blk ^. BlockId ==. tx ^. TxBlockId)
            txOutSpentP txOut
            -- Only the initial genesis block has a size of 0.
            where_ (blk ^. BlockSize ==. val 0)
            pure countRows
  pure $ maybe 0 (dividePageSize pageSize) (listToMaybe res)

queryUnRedeemedGenesisAddressCount :: MonadIO m => PageSize -> SqlPersistT m Word
queryUnRedeemedGenesisAddressCount (PageSize pageSize) = do
  res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            on (blk ^. BlockId ==. tx ^. TxBlockId)
            txOutUnspentP txOut
            -- Only the initial genesis block has a size of 0.
            where_ (blk ^. BlockSize ==. val 0)
            pure countRows
  pure $ maybe 0 (dividePageSize pageSize) (listToMaybe res)


dividePageSize :: Word -> Value Word -> Word
dividePageSize pageSize vw =
  divRoundUp (unValue vw) pageSize
