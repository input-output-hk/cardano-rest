{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Api.Legacy.TxLast
  ( getLastTxs
  ) where

import Cardano.Db
    ( EntityField (..), Tx, isJust )
import Control.Monad.IO.Class
    ( MonadIO )
import Data.ByteString.Char8
    ( ByteString )
import Data.Fixed
    ( Fixed (..), Uni )
import Data.Time.Clock
    ( UTCTime )
import Data.Time.Clock.POSIX
    ( utcTimeToPOSIXSeconds )
import Database.Esqueleto
    ( Entity
    , InnerJoin (..)
    , SqlExpr
    , Value
    , desc
    , from
    , limit
    , on
    , orderBy
    , select
    , subSelectUnsafe
    , sum_
    , unValue
    , where_
    , (==.)
    , (^.)
    )
import Database.Persist.Sql
    ( SqlPersistT )
import Explorer.Web.Api.Legacy.Util
import Explorer.Web.ClientTypes
    ( CHash (..), CTxEntry (..), CTxHash (..) )
import Explorer.Web.Error
    ( ExplorerError (..) )


getLastTxs :: MonadIO m => SqlPersistT m (Either ExplorerError [CTxEntry])
getLastTxs = Right <$> queryCTxEntry


queryCTxEntry :: MonadIO m => SqlPersistT m [CTxEntry]
queryCTxEntry = do
    txRows <- select . from $ \ (blk `InnerJoin` tx) -> do
                on (blk ^. BlockId ==. tx ^. TxBlock)
                where_ (isJust $ blk ^. BlockSlotNo)
                orderBy [desc (blk ^. BlockSlotNo)]
                limit 20
                pure (blk ^. BlockTime, tx ^. TxHash, txOutValue tx)
    pure $ map convert txRows
  where
    convert :: (Value UTCTime, Value ByteString, Value (Maybe Uni)) -> CTxEntry
    convert (vtime, vhash, vtotal) =
      CTxEntry
        { cteId = CTxHash . CHash $ bsBase16Encode (unValue vhash)
        , cteTimeIssued = Just $ utcTimeToPOSIXSeconds (unValue vtime)
        , cteAmount = unTotal vtotal
        }

txOutValue :: SqlExpr (Entity Tx) -> SqlExpr (Value (Maybe Uni))
txOutValue tx =
  -- This actually is safe.
  subSelectUnsafe . from $ \ txOut -> do
    where_ (txOut ^. TxOutTxId ==. tx ^. TxId)
    pure $ sum_ (txOut ^. TxOutValue)

unTotal :: Num a => Value (Maybe Uni) -> a
unTotal mvi =
  fromIntegral $
  case unValue mvi of
    Just (MkFixed x) -> x
    _ -> 0
