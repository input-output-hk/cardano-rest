{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Cardano.TxSubmit.Types
  ( TxSubmitApi
  , TxSubmitApiRecord (..)
  , TxSubmitError (..)
  , TxSubmitPort (..)
  , renderTxSubmitError
  ) where

import Cardano.Binary
    ( DecoderError )
import Cardano.TxSubmit.ErrorRender

import Cardano.Chain.Byron.API
    ( ApplyMempoolPayloadErr (..) )
import Data.Aeson
    ( ToJSON (..), Value (..) )
import Data.ByteString.Char8
    ( ByteString )
import Data.Text
    ( Text )
import Formatting
    ( build, sformat )
import GHC.Generics
    ( Generic )
import Network.HTTP.Media
    ( (//) )
import Servant
    ( (:>)
    , Accept (..)
    , JSON
    , MimeRender (..)
    , MimeUnrender (..)
    , PostAccepted
    , ReqBody
    )
import Servant.API.Generic
    ( (:-), ToServantApi )

import qualified Cardano.Chain.UTxO as Utxo
import qualified Data.ByteString.Lazy.Char8 as LBS

newtype TxSubmitPort
  = TxSubmitPort Int

data TxSubmitError
  = TxSubmitDecodeHex
  | TxSubmitEmpty
  | TxSubmitDecodeFail DecoderError
  | TxSubmitBadTx Text
  | TxSubmitFail ApplyMempoolPayloadErr
  deriving Eq

instance ToJSON TxSubmitError where
  toJSON = convertJson

convertJson :: TxSubmitError -> Value
convertJson = String . renderTxSubmitError

renderTxSubmitError :: TxSubmitError -> Text
renderTxSubmitError st =
  case st of
    TxSubmitDecodeHex -> "Provided data was hex encoded and this webapi expects raw binary"
    TxSubmitEmpty -> "Provided transaction has zero length"
    TxSubmitDecodeFail err -> sformat build err
    TxSubmitBadTx tt -> mconcat ["Transactions of type '", tt, "' not supported"]
    TxSubmitFail err -> renderApplyMempoolPayloadErr err

-- | Servant API which provides access to tx submission webapi
type TxSubmitApi
    = "api" :> ToServantApi TxSubmitApiRecord

-- | A servant-generic record with all the methods of the API
data TxSubmitApiRecord route = TxSubmitApiRecord
  { _txSubmitPost :: route
        :- "submit"
        :> "tx"
        :> ReqBody '[CBORStream] ByteString
        :> PostAccepted '[JSON] Utxo.TxId
  } deriving (Generic)

data CBORStream

instance Accept CBORStream where
  contentType _ = "application" // "cbor"

instance MimeRender CBORStream ByteString where
    mimeRender _ = LBS.fromStrict

instance MimeRender CBORStream LBS.ByteString where
    mimeRender _ = id

instance MimeUnrender CBORStream ByteString where
    mimeUnrender _ = Right . LBS.toStrict

instance MimeUnrender CBORStream LBS.ByteString where
    mimeUnrender _ = Right . id
