{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Api.Legacy.GenesisSummary
  ( genesisSummary
  ) where

import Cardano.Db
    ( EntityField (..), txOutSpentP )
import Control.Monad.IO.Class
    ( MonadIO )
import Data.Fixed
    ( Fixed (..), Uni )
import Data.Maybe
    ( listToMaybe )
import Database.Esqueleto
    ( InnerJoin (..)
    , Value
    , countRows
    , from
    , on
    , select
    , sum_
    , unValue
    , val
    , where_
    , (==.)
    , (^.)
    )
import Database.Persist.Sql
    ( SqlPersistT )
import Explorer.Web.ClientTypes
    ( CGenesisSummary (..) )
import Explorer.Web.Error
    ( ExplorerError (..) )

genesisSummary :: MonadIO m => SqlPersistT m (Either ExplorerError CGenesisSummary)
genesisSummary = Right <$> do
    (numTotal,valTotal) <- queryInitialGenesis
    (redTotal, valRedeemed) <- queryGenesisRedeemed
    pure $ CGenesisSummary
            { cgsNumTotal = numTotal
            , cgsNumRedeemed = redTotal
            , cgsNumNotRedeemed = numTotal - redTotal
            , cgsRedeemedAmountTotal = fromIntegral valRedeemed
            , cgsNonRedeemedAmountTotal = fromIntegral $ valTotal - valRedeemed
            }

queryInitialGenesis :: MonadIO m => SqlPersistT m (Word, Integer)
queryInitialGenesis = do
    res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
              on (tx ^. TxId ==. txOut ^. TxOutTxId)
              on (blk ^. BlockId ==. tx ^. TxBlockId)
              -- Only the initial genesis block has a size of 0.
              where_ (blk ^. BlockSize ==. val 0)
              pure (countRows, sum_ (txOut ^. TxOutValue))
    pure $ maybe (0, 0) convertPair (listToMaybe res)

queryGenesisRedeemed :: MonadIO m => SqlPersistT m (Word, Integer)
queryGenesisRedeemed = do
    res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
              on (tx ^. TxId ==. txOut ^. TxOutTxId)
              on (blk ^. BlockId ==. tx ^. TxBlockId)
              txOutSpentP txOut
              -- Only the initial genesis block has a size of 0.
              where_ (blk ^. BlockSize ==. val 0)
              pure (countRows, sum_ (txOut ^. TxOutValue))
    pure $ maybe (0, 0) convertPair (listToMaybe res)

convertPair :: (Value Word, Value (Maybe Uni)) -> (Word, Integer)
convertPair (vcount, vtotal) = (unValue vcount, unTotal vtotal)

unTotal :: Value (Maybe Uni) -> Integer
unTotal mvi =
  case unValue mvi of
    Just (MkFixed x) -> x
    _ -> 0
