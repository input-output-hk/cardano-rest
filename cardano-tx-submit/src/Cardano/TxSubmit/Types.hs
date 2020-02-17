{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Cardano.TxSubmit.Types
  ( TxSubmitApi
  , TxSubmitApiRecord (..)
  , TxSubmitStatus (..)
  , TxSubmitPort (..)
  , renderTxSubmitStatus
  ) where

import           Cardano.Binary (DecoderError)
import qualified Cardano.Chain.UTxO as Utxo
import           Cardano.TxSubmit.ErrorRender

import           Data.Aeson (ToJSON (..), Value (..))
import qualified Data.Aeson as Aeson
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)

import           Formatting ((%), build, sformat)

import           GHC.Generics (Generic)

import           Network.HTTP.Media ((//))

import           Ouroboros.Consensus.Ledger.Byron.Auxiliary (ApplyMempoolPayloadErr)

import           Servant (Accept (..), JSON, MimeRender (..), MimeUnrender (..), Post, ReqBody, (:>))
import           Servant.API.Generic (ToServantApi, (:-))

newtype TxSubmitPort
  = TxSubmitPort Int

data TxSubmitStatus
  = TxSubmitOk Utxo.TxId
  | TxSubmitDecodeHex
  | TxSubmitEmpty
  | TxSubmitDecodeFail DecoderError
  | TxSubmitBadTx Text
  | TxSubmitFail ApplyMempoolPayloadErr
  deriving Eq

instance ToJSON TxSubmitStatus where
  toJSON = convertJson

convertJson :: TxSubmitStatus -> Value
convertJson st =
    Aeson.object
      [ ( "status", String statusMsg )
      , ( "message", String (renderTxSubmitStatus st) )
      ]
  where
    statusMsg :: Text
    statusMsg =
      case st of
        TxSubmitOk{} -> "success"
        _other -> "fail"

renderTxSubmitStatus :: TxSubmitStatus -> Text
renderTxSubmitStatus st =
  case st of
    TxSubmitOk tx -> sformat ("Tx "% build %" submitted successfully") tx
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
        :> Post '[JSON] TxSubmitStatus
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
