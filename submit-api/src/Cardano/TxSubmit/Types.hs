{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Cardano.TxSubmit.Types
  ( TxSubmitApi
  , TxSubmitApiRecord (..)
  , TxSubmitWebApiError (..)
  , TxSubmitPort (..)
  , renderTxSubmitWebApiError
  ) where

import Cardano.Api.Typed
    ( TxId )
import Cardano.Binary
    ( DecoderError )
import Cardano.TxSubmit.Tx
    ( TxSubmitError, renderTxSubmitError )
import Control.Arrow
    ( left )
import Data.Aeson
    ( ToJSON (..), Value (..) )
import Data.Bifunctor
    ( bimap )
import Data.ByteString.Base16
    ( decodeBase16 )
import Data.ByteString.Char8
    ( ByteString )
import Data.ByteString.Lazy.Base64
    ( decodeBase64 )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Text
    ( Text )
import Formatting
    ( build, sformat )
import GHC.Generics
    ( Generic )
import Network.HTTP.Media
    ( parameters, (//), (/:) )
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

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

newtype TxSubmitPort
  = TxSubmitPort Int

-- | An error that can occur in the transaction submission web API.
data TxSubmitWebApiError
  = TxSubmitDecodeHex
  | TxSubmitEmpty
  | TxSubmitDecodeFail !DecoderError
  | TxSubmitBadTx !Text
  | TxSubmitFail !TxSubmitError
  deriving Eq

instance ToJSON TxSubmitWebApiError where
  toJSON = convertJson

convertJson :: TxSubmitWebApiError -> Value
convertJson = String . renderTxSubmitWebApiError

renderTxSubmitWebApiError :: TxSubmitWebApiError -> Text
renderTxSubmitWebApiError st =
  case st of
    TxSubmitDecodeHex -> "Provided data was hex encoded and this webapi expects raw binary"
    TxSubmitEmpty -> "Provided transaction has zero length"
    TxSubmitDecodeFail err -> sformat build err
    TxSubmitBadTx tt -> mconcat ["Transactions of type '", tt, "' not supported"]
    TxSubmitFail err -> renderTxSubmitError err

-- | Servant API which provides access to tx submission webapi
type TxSubmitApi
    = "api" :> ToServantApi TxSubmitApiRecord

-- | A servant-generic record with all the methods of the API
data TxSubmitApiRecord route = TxSubmitApiRecord
  { _txSubmitPost :: route
        :- "submit"
        :> "tx"
        :> ReqBody '[CBORStream] ByteString
        :> PostAccepted '[JSON] TxId
  } deriving (Generic)

data CBORStream

instance Accept CBORStream where
  contentTypes _ = "application" // "cbor" /: ("encoding", "base16") :|
    [ "application" // "cbor" /: ("encoding", "base64")
    , "application" // "cbor"
    ]

instance MimeRender CBORStream ByteString where
    mimeRender _ = LBS.fromStrict

instance MimeRender CBORStream LBS.ByteString where
    mimeRender _ = id

instance MimeUnrender CBORStream ByteString where
    mimeUnrender proxy = fmap LBS.toStrict . mimeUnrender proxy

instance MimeUnrender CBORStream LBS.ByteString where
    mimeUnrenderWithType _ mediaType bytes =
        case Map.lookup "encoding" (parameters mediaType) of
            Nothing ->
                Right bytes
            Just "base16" ->
                bimap T.unpack LBS.fromStrict . decodeBase16 . LBS.toStrict $ bytes
            Just "base64" ->
                left T.unpack . decodeBase64 $ bytes
            Just _ ->
                Left "unrecognized encoding: must be 'base16' or 'base64' if specified."
