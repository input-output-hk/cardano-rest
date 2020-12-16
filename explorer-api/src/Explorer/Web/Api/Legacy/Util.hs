{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Api.Legacy.Util
  ( blockPosixTime
  , bsBase16Encode
  , collapseTxGroup
  , decodeTextAddress
  , defaultPageSize
  , divRoundUp
  , genesisDistributionTxHash
  , k
  , runQuery
  , textBase16Decode
  , textShow
  , toPageSize
  , zipTxBrief
  , isRedeemAddress
  , jsonToMetadataValue
  ) where

import Cardano.Db
    ( Block (..), TxId )
import Data.Int (Int64)
import Data.Scientific (toBoundedInteger)
import Cardano.Ledger.Shelley
    ( Shelley, ShelleyEra )
import Cardano.Api.MetaData (TxMetadataValue (..))

import Control.Applicative
    ( (<|>) )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase )
import Data.ByteString
    ( ByteString )
import qualified Data.Vector as Vector
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe, mapMaybe )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( UTCTime )
import Data.Time.Clock.POSIX
    ( POSIXTime, utcTimeToPOSIXSeconds )
import Data.Word
    ( Word64 )
import Database.Persist.Sql
    ( IsolationLevel (..), SqlBackend, SqlPersistT, runSqlConnWithIsolation )
import Explorer.Web.Api.Legacy.Types
    ( PageSize (..) )
import Explorer.Web.ClientTypes
    ( CHash (..), CTxAddressBrief (..), CTxBrief (..), CTxHash (..) )
import Explorer.Web.Error
    ( ExplorerError (..) )
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( StandardCrypto )
import Shelley.Spec.Ledger.Address
    ( Addr (..), BootstrapAddress (..), deserialiseAddr )

import qualified Cardano.Chain.Common as Byron
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.HashMap.Strict as HM

import qualified Data.Aeson as Aeson 

blockPosixTime :: Block -> POSIXTime
blockPosixTime = utcTimeToPOSIXSeconds . blockTime

-- | bsBase16Encode : Convert a raw ByteString to Base16 and then encode it as Text.
bsBase16Encode :: ByteString -> Text
bsBase16Encode bs =
  case Text.decodeUtf8' (Base16.encode bs) of
    Left _ -> Text.pack $ "UTF-8 decode failed for " ++ show bs
    Right txt -> txt

collapseTxGroup :: [(TxId, a)] -> (TxId, [a])
collapseTxGroup xs =
  case xs of
    [] -> error "collapseTxGroup: groupOn produced [] on non-empty list (impossible)"
    (x:_) -> (fst x, map snd xs)

decodeTextAddress :: Text -> Either ExplorerError (Addr (ShelleyEra StandardCrypto))
decodeTextAddress txt =
    case tryBase16 <|> tryBech32 <|> tryBase58 of
        Nothing ->
            Left $ Internal "Invalid address encoding. Neither Base16, Bech32 nor Base58."
        Just bytes ->
            case deserialiseAddr bytes of
                Nothing ->
                    Left $ Internal "Invalid address."
                Just addr ->
                    Right addr
  where
    -- | Attempt decoding an 'Address' using Base16 encoding
    tryBase16 :: Maybe ByteString
    tryBase16 =
        either (const Nothing) Just $ convertFromBase Base16 (Text.encodeUtf8 txt)

    -- | Attempt decoding an 'Address' using a Bech32 encoding.
    tryBech32 :: Maybe ByteString
    tryBech32 = do
        (_, dp) <- either (const Nothing) Just (Bech32.decodeLenient txt)
        Bech32.dataPartToBytes dp

    -- | Attempt decoding a legacy 'Address' using a Base58 encoding.
    tryBase58 :: Maybe ByteString
    tryBase58 =
        decodeBase58 bitcoinAlphabet (Text.encodeUtf8 txt)

isRedeemAddress :: Addr crypto -> Bool
isRedeemAddress = \case
    Addr{} -> False
    AddrBootstrap (BootstrapAddress addr) -> Byron.isRedeemAddress addr

defaultPageSize :: PageSize
defaultPageSize = PageSize 10

divRoundUp :: Integral a => a -> a -> a
divRoundUp a b = (a + b - 1) `div` b

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

genesisDistributionTxHash :: CTxHash
genesisDistributionTxHash = CTxHash (CHash "Genesis Distribution")

-- TODO, get this from the config somehow
k :: Word64
k = 2160

runQuery :: MonadIO m => SqlBackend -> SqlPersistT IO a -> m a
runQuery backend query =
  liftIO $ runSqlConnWithIsolation query backend Serializable

textBase16Decode :: Text -> Either ExplorerError ByteString
textBase16Decode text = do
  case Base16.decode (Text.encodeUtf8 text) of
    Right bs -> Right bs
    Left {}  -> Left $ Internal (Text.pack $ "Unable to Base16.decode " ++ show text ++ ".")

textShow :: Show a => a -> Text
textShow = Text.pack . show

toPageSize :: Maybe PageSize -> PageSize
toPageSize = fromMaybe defaultPageSize

zipTxBrief :: [(TxId, ByteString, UTCTime)] -> [(TxId, [CTxAddressBrief])] -> [(TxId, [CTxAddressBrief])] -> [CTxBrief]
zipTxBrief xs ins outs =
    mapMaybe (build . fst3) xs
  where
    idMap :: Map TxId (ByteString, UTCTime)
    idMap = Map.fromList $ map (\(a, b, c) -> (a, (b, c))) xs

    inMap :: Map TxId [CTxAddressBrief]
    inMap = Map.fromList ins

    outMap :: Map TxId [CTxAddressBrief]
    outMap = Map.fromList outs

    build :: TxId -> Maybe CTxBrief
    build txid = do
      (hash, time) <- Map.lookup txid idMap
      inputs <- Map.lookup txid inMap
      outputs <- Map.lookup txid outMap
      inSum <- Just $ sum (map ctaAmount inputs)
      outSum <- Just $ sum (map ctaAmount outputs)
      pure $ CTxBrief
              { ctbId = CTxHash . CHash $ bsBase16Encode hash
              , ctbTimeIssued = Just $ utcTimeToPOSIXSeconds time
              , ctbInputs = inputs
              , ctbOutputs = outputs
              , ctbInputSum = inSum
              , ctbOutputSum = outSum
              , ctbFees = inSum - outSum
              }

-- TODO: This function and the corresponding one in `cardano-db-sync` repo under 
--   cardano-db-sync/src/Cardano/DbSync/Era/Shelley/Metadata.hs should probably get
--   grouped together in a shared dependency (e.g. in cardano-db).

-- | Converts an Aeson Value to a TxMetadataValue, returning
-- Nothing if conversion logic fails. This does not perfectly
-- recreate the input of `Cardano.DbSync.Era.Shelley.Generic.Metadata.metadataValueToJsonNoSchema`
-- since that function applies a irreversible transformation to map keys. 
-- Instead, all TxMetaMap keys are treated as Text.
jsonToMetadataValue :: Aeson.Value -> Maybe TxMetadataValue
jsonToMetadataValue = conv 
  where 
      conv :: Aeson.Value -> Maybe TxMetadataValue
      conv (Aeson.Number n) = case (toBoundedInteger n :: Maybe Int64) of 
          Nothing -> Nothing 
          Just int -> Just $ TxMetaNumber $ fromIntegral int
      conv (Aeson.String s) = case Text.stripPrefix bytesPrefix s of
          Nothing -> Just $ TxMetaText s 
          Just encodedText -> case Base16.decode $ Char8.pack $ Text.unpack encodedText of
              (valid, "") -> Just $ TxMetaBytes valid
              -- Invalid base16 bytestrings are just returned as text
              _ -> Just $ TxMetaText s
      conv (Aeson.Array a) = Just $ TxMetaList $ mapMaybe conv (Vector.toList a)
      -- Note: No way of knowing the key type since all are serialized straight to text
      --   with no extra type identifiers. Have to just return them as text.
      conv (Aeson.Object obj) = Just $ TxMetaMap $ mapMaybe convMapEntry (HM.toList obj)
      conv _ = Nothing

      convMapEntry :: (Text, Aeson.Value) -> Maybe (TxMetadataValue, TxMetadataValue)
      convMapEntry (oldKey, oldValue) = let
        newKey = conv (Aeson.String oldKey)
        newValue = conv oldValue 
        in case newKey of 
            Nothing -> Nothing 
            Just key -> case newValue of 
                Nothing -> Nothing 
                Just value -> Just (key, value)

-- | JSON strings that are base16 encoded and prefixed with 'bytesPrefix' will
-- be encoded as CBOR bytestrings.
bytesPrefix :: Text
bytesPrefix = "0x"
