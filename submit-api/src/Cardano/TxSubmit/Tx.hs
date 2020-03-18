{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.Tx
  ( TxSubmitVar (..)
  , newTxSubmitVar
  , readTxSubmit
  , writeTxSubmitResponse
  , submitTx
  ) where

import Cardano.Prelude hiding
    ( atomically )

import Cardano.Chain.Byron.API
    ( ApplyMempoolPayloadErr (..) )
import Cardano.Chain.UTxO
    ( TxId )
import Cardano.TxSubmit.Types
import Control.Monad.Class.MonadSTM.Strict
    ( StrictTMVar, atomically, newEmptyTMVarM, putTMVar, takeTMVar )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock (..), GenTx (..) )

-- The type of 'reject' (determined by ouroboros-network) is currently 'Maybe String'.
-- Hopefully that will be fixed to make it a concrete type.
-- See: https://github.com/input-output-hk/ouroboros-network/issues/1335
data TxSubmitVar = TxSubmitVar
  { txSubmit :: !(StrictTMVar IO (GenTx ByronBlock))
  , txRespond :: !(StrictTMVar IO (Maybe ApplyMempoolPayloadErr))
  }

newTxSubmitVar :: IO (TxSubmitVar)
newTxSubmitVar =
  TxSubmitVar <$> newEmptyTMVarM <*> newEmptyTMVarM

-- | Read a previously submitted tx from the TMVar.
readTxSubmit :: TxSubmitVar -> IO (GenTx ByronBlock)
readTxSubmit tsv =
  atomically $ takeTMVar (txSubmit tsv)

-- | Write the response recieved when tx has been submitted.
writeTxSubmitResponse :: TxSubmitVar -> (Maybe ApplyMempoolPayloadErr) -> IO ()
writeTxSubmitResponse tsv merr =
  atomically $ putTMVar (txRespond tsv) merr

-- | Submit a tx and wait for the response. This is done as a pair of atomic
-- operations, to allow the tx to be read in one operation, submmited and then
-- the response written as a second operation. Doing this as a single atmomic
-- operation would not work as the other end of the submit/response pair need
-- to be operated on independently.
submitTx :: TxSubmitVar -> GenTx ByronBlock -> IO (Either TxSubmitError TxId)
submitTx tsv tx =
  case tx of
    ByronTx txid _ -> do
      atomically $ putTMVar (txSubmit tsv) tx
      maybe (Right txid) (Left . TxSubmitFail) <$> atomically (takeTMVar $ txRespond tsv)
    ByronDlg {} -> pure $ Left $ TxSubmitBadTx "Delegation"
    ByronUpdateProposal {} -> pure $ Left $ TxSubmitBadTx "Proposal"
    ByronUpdateVote {} -> pure $ Left $ TxSubmitBadTx "UpdateVote"
