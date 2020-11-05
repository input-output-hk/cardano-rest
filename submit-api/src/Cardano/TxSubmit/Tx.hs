{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.TxSubmit.Tx
  ( TxSubmitError (..)
  , renderTxSubmitError
  , submitTx
  ) where

import Cardano.Prelude

import Cardano.Api.TxSubmit hiding
    ( submitTx )
import Cardano.Api.Typed
    ( Byron
    , LocalNodeConnectInfo
    , NodeConsensusMode (..)
    , Shelley
    , Tx (..)
    , TxBody (..)
    , TxId
    , getTxId
    , localNodeConsensusMode
    )
import Cardano.TxSubmit.ErrorRender
    ( renderApplyMempoolPayloadErr, renderEraMismatch )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock )
import Ouroboros.Consensus.Cardano.Block
    ( EraMismatch (..)
    , HardForkApplyTxErr (ApplyTxErrByron, ApplyTxErrShelley, ApplyTxErrWrongEra)
    )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( ApplyTxErr )
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyBlock )
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( StandardCrypto )

import qualified Cardano.Api.TxSubmit as Api
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Ledger.Shelley as Era
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley

-- | An error that can occur while submitting a transaction to a local node.
data TxSubmitError
  = TxSubmitByronError !(ApplyTxErr ByronBlock)
  | TxSubmitShelleyError !(ApplyTxErr (ShelleyBlock (Era.Shelley StandardCrypto)))
  | TxSubmitEraMismatchError !EraMismatch
  deriving (Eq, Show)

renderTxSubmitError :: TxSubmitError -> Text
renderTxSubmitError tse =
  case tse of
    TxSubmitByronError err -> renderApplyMempoolPayloadErr err
    TxSubmitShelleyError err -> show err -- TODO: Better rendering for Shelley errors
    TxSubmitEraMismatchError err -> renderEraMismatch err

-- | Submit a transaction to a local node.
submitTx
  :: forall mode block.
     LocalNodeConnectInfo mode block
  -> (Either (Tx Byron) (Tx Shelley))
  -> IO (Either TxSubmitError TxId)
submitTx connectInfo byronOrShelleyTx =
  case (localNodeConsensusMode connectInfo, byronOrShelleyTx) of
    (ByronMode{}, Left tx) -> do
      result <- liftIO $ Api.submitTx connectInfo (TxForByronMode tx)
      pure $ case result of
        TxSubmitSuccess -> Right (getTxIdForTx tx)
        TxSubmitFailureByronMode err -> Left (TxSubmitByronError err)

    (ByronMode{}, Right{}) ->
      pure $ Left $ TxSubmitEraMismatchError EraMismatch {
                ledgerEraName = "Byron",
                otherEraName  = "Shelley"
              }

    (ShelleyMode{}, Right tx) -> do
      result <- liftIO $ Api.submitTx connectInfo (TxForShelleyMode tx)
      case result of
        TxSubmitSuccess -> pure $ Right (getTxIdForTx tx)
        TxSubmitFailureShelleyMode err ->
          pure $ Left (TxSubmitShelleyError err)

    (ShelleyMode{}, Left{}) ->
      pure $ Left $ TxSubmitEraMismatchError EraMismatch {
                ledgerEraName = "Shelley",
                otherEraName  = "Byron"
              }

    (CardanoMode{}, tx) -> do
      result <- Api.submitTx connectInfo (TxForCardanoMode tx)
      pure $ case result of
        TxSubmitSuccess -> Right (either getTxIdForTx getTxIdForTx tx)
        TxSubmitFailureCardanoMode (ApplyTxErrByron err) ->
          Left (TxSubmitByronError err)
        TxSubmitFailureCardanoMode (ApplyTxErrShelley err) ->
          Left (TxSubmitShelleyError err)
        TxSubmitFailureCardanoMode (ApplyTxErrWrongEra mismatch) ->
          Left (TxSubmitEraMismatchError mismatch)

-- TODO: This function should really be implemented in `Cardano.Api.Typed`.
-- The function, 'Cardano.Api.Typed.getTxId', accepts a 'TxBody' parameter.
getTxIdForTx :: Tx era -> TxId
getTxIdForTx (ByronTx tx) = getTxId . ByronTxBody . Byron.aTaTx $ tx
getTxIdForTx (ShelleyTx tx) =
  getTxId . (uncurry ShelleyTxBody) $
    (Shelley._body tx, Shelley.strictMaybeToMaybe (Shelley._metadata tx))
