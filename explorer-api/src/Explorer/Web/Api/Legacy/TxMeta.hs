{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Api.Legacy.TxMeta
  ( txMeta,
  )
where

import Cardano.Api.Typed
  ( TxMetadata (..),
    TxMetadataValue (..),
  )
import Cardano.Db (DbWord64 (..), EntityField (..))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Database.Esqueleto
  ( InnerJoin (..),
    Value (..),
    asc,
    from,
    on,
    orderBy,
    select,
    val,
    where_,
    (==.),
    (^.),
  )
import Database.Persist.Sql (SqlPersistT)
import Explorer.Web.Api.Legacy.Util (jsonToMetadataValue, textBase16Decode)
import Explorer.Web.ClientTypes
  ( CHash (..),
    CTxHash (..),
    CTxMeta (..),
    CTxMetaValues (..),
  )
import Explorer.Web.Error (ExplorerError (..))

-- | Gets the transaction metadata for a given transaction. Records
-- which cannot be decoded will be ignored.
txMeta :: MonadIO m => CTxHash -> SqlPersistT m (Either ExplorerError CTxMeta)
txMeta = queryCTxMeta

queryCTxMeta :: MonadIO m => CTxHash -> SqlPersistT m (Either ExplorerError CTxMeta)
queryCTxMeta cHash = do
  let (CTxHash (CHash hashTxt)) = cHash
      hashBytes = textBase16Decode hashTxt
  case hashBytes of
    Left e -> pure $ Left e
    Right hash -> do
      rows <- select . from $ \(meta `InnerJoin` tx) -> do
        on (meta ^. TxMetadataTxId ==. tx ^. TxId)
        where_ (tx ^. TxHash ==. val hash)
        orderBy [asc (meta ^. TxMetadataKey)]
        return (meta ^. TxMetadataKey, meta ^. TxMetadataJson)
      pure $
        Right
          CTxMeta
            { ctmTxId = cHash,
              ctmData = CTxMetaValues $ TxMetadata $ Map.fromList $ mapMaybe (decode >=> convert) rows
            }
  where
    decode :: (Value DbWord64, Value T.Text) -> Maybe (Word64, Aeson.Value)
    decode (Value (DbWord64 key), Value json) = case Aeson.decodeStrict $ encodeUtf8 json of
      Nothing -> Nothing
      Just value -> Just (key, value)

    convert :: (Word64, Aeson.Value) -> Maybe (Word64, TxMetadataValue)
    convert (key, value) = case jsonToMetadataValue value of
      Nothing -> Nothing
      Just metaVal -> Just (key, metaVal)
