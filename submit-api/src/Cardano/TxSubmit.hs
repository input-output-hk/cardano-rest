{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit
  ( module X
  , runTxSubmitWebapi
  ) where

import qualified Cardano.BM.Setup as Logging
import qualified Cardano.BM.Trace as Logging
import           Cardano.BM.Trace (Trace, logInfo)

import           Cardano.Prelude

import           Cardano.TxSubmit.Config as X
import           Cardano.TxSubmit.Node as X
import           Cardano.TxSubmit.Parsers as X
import           Cardano.TxSubmit.Tx as X
import           Cardano.TxSubmit.Types as X
import           Cardano.TxSubmit.Util as X
import           Cardano.TxSubmit.Web as X

import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Class (liftIO)

import           Data.Text (Text)


runTxSubmitWebapi :: TxSubmitNodeParams -> IO ()
runTxSubmitWebapi tsnp = do
    tsnc <- readTxSubmitNodeConfig (unConfigFile $ tspConfigFile tsnp)
    trce <- mkTracer tsnc
    tsv <- newTxSubmitVar
    Async.race_
      (runTxSubmitNode tsv trce tspProtocol tspNetworkId tspSocketPath)
      (runTxSubmitServer tsv trce (tspWebPort tsnp))
    logInfo trce "runTxSubmitWebapi: Async.race_ returned"
  where
    TxSubmitNodeParams
      { tspProtocol
      , tspNetworkId
      , tspSocketPath
      } = tsnp

mkTracer :: TxSubmitNodeConfig -> IO (Trace IO Text)
mkTracer enc =
  if not (tscEnableLogging enc)
    then pure Logging.nullTracer
    else liftIO $ Logging.setupTrace (Right $ tscLoggingConfig enc) "cardano-tx-submit"
