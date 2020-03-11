{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.TxSubmit.Web
  ( runTxSubmitServer
  ) where

import Cardano.TxSubmit.Tx
import Cardano.TxSubmit.Types
import Cardano.TxSubmit.Util

import Cardano.Binary
    ( DecoderError )
import Cardano.BM.Trace
    ( Trace, logInfo )
import Cardano.Chain.UTxO
    ( TxId )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Aeson
    ( ToJSON (..) )
import Data.ByteString.Char8
    ( ByteString )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Formatting
    ( build, sformat )
import Ouroboros.Consensus.Ledger.Byron
    ( ByronBlock, GenTx )
import Servant
    ( Application, Handler, ServerError (..), err400, throwError )
import Servant.API.Generic
    ( toServant )
import Servant.Server.Generic
    ( AsServerT )

import qualified Cardano.Binary as Binary
import qualified Cardano.Chain.UTxO as Ledger
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Char as Char
import qualified Network.Wai.Handler.Warp as Warp
import qualified Ouroboros.Consensus.Ledger.Byron as Byron
import qualified Servant

runTxSubmitServer :: TxSubmitVar-> Trace IO Text -> TxSubmitPort -> IO ()
runTxSubmitServer tsv trce (TxSubmitPort port) = do
  logInfo trce $ "Running tx-submit web server on http://localhost:" <> textShow port <> "/"
  logException trce "tx-submit-webapi." $
    Warp.run port (txSubmitApp tsv trce)
  logInfo trce "txSubmitApp: exiting"

txSubmitApp :: TxSubmitVar-> Trace IO Text -> Application
txSubmitApp tsv trce =
    Servant.serve (Proxy :: Proxy TxSubmitApi) (toServant handlers)
  where
    handlers :: TxSubmitApiRecord (AsServerT Handler)
    handlers = TxSubmitApiRecord
      { _txSubmitPost = txSubmitPost tsv trce
      }

txSubmitPost
    :: TxSubmitVar -> Trace IO Text -> ByteString
    -> Handler TxId
txSubmitPost tsv trce tx = do
  liftIO $ logInfo trce ("txSubmitPost: received " <> textShow (BS.length tx) <> " bytes")
  case decodeByronTx tx of
    Left err -> do
      let serr = if
                  | BS.length tx == 0 -> TxSubmitEmpty
                  | BS.all isHexOrWhitespace tx -> TxSubmitDecodeHex
                  | otherwise -> TxSubmitDecodeFail err
      liftIO $ logInfo trce $ "txSubmitPost failed: "
        <> renderTxSubmitError serr
      errorResponse serr
    Right tx1 -> do
      resp <- liftIO $ submitTx tsv tx1
      liftIO $ logInfo trce $ "txSubmitPost: "
        <> either renderTxSubmitError (sformat build) resp
      either errorResponse pure resp
  where
    errorResponse :: ToJSON e => e -> Handler a
    errorResponse e = throwError $ err400 { errBody = Aeson.encode e }

decodeByronTx :: ByteString -> Either DecoderError (GenTx ByronBlock)
decodeByronTx bs =
    toGenTx <$> fromCborTxAux bs
  where
    toGenTx :: Ledger.ATxAux ByteString -> GenTx ByronBlock
    toGenTx tx = Byron.ByronTx (Byron.byronIdTx tx) tx

--TODO: remove this local definition of this function as soon as the updated
-- ledger lib with Byron.fromCborTxAux is available
fromCborTxAux :: ByteString ->  Either DecoderError (Ledger.ATxAux ByteString)
fromCborTxAux bs =
    fmap (annotationBytes lbs)
      $ Binary.decodeFullDecoder "Cardano.Chain.UTxO.TxAux.fromCborTxAux"
                                 Binary.fromCBOR lbs
  where
    lbs = LBS.fromStrict bs

    annotationBytes :: Functor f => LBS.ByteString -> f Binary.ByteSpan -> f ByteString
    annotationBytes bytes = fmap (LBS.toStrict . Binary.slice bytes)

isHexOrWhitespace :: Char -> Bool
isHexOrWhitespace c = Char.isHexDigit c || Char.isSpace c
