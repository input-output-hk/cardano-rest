{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Api.Legacy.TxMeta
  ( txMeta,
  )
where

import Cardano.Db (EntityField (..))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (fromList)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
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
import Explorer.Web.Api.Legacy.Util (textBase16Decode)
import Explorer.Web.ClientTypes
  ( CHash (..),
    CTxHash (..),
    CTxMeta (..),
  )
import Explorer.Web.Error (ExplorerError (..))

-- | Gets the transaction metadata for a given transaction. Records
-- which cannot be decoded as a JSON Value will be ignored.
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
        orderBy [asc (meta ^. TxMetadataId)]
        return $ meta ^. TxMetadataJson
      let indexedValues = zip [0 ..] $ mapMaybe decode rows :: [(Integer, Aeson.Value)]
          obj = fromList $ map (\(i, v) -> (T.pack $ show i, v)) indexedValues
      pure $ Right CTxMeta {ctmTxId = cHash, ctmJSON = obj}
  where
    decode :: Value T.Text -> Maybe Aeson.Value
    decode (Value json) = Aeson.decodeStrict $ encodeUtf8 json
