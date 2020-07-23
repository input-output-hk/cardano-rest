{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Api.Legacy.AddressSummary
  ( addressSummary

  -- For testing.
  , queryAddressSummary
  ) where

import Cardano.Chain.Common
    ( Address, isRedeemAddress )
import Cardano.Db
    ( EntityField (..), TxId, unValue3 )
import Control.Monad.IO.Class
    ( MonadIO )
import Control.Monad.Trans.Except.Extra
    ( hoistEither, newExceptT, runExceptT )
import Data.ByteString.Char8
    ( ByteString )
import Data.List.Extra
    ( groupOn )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( UTCTime )
import Data.Word
    ( Word16, Word64 )
import Database.Esqueleto
    ( InnerJoin (..)
    , Value (..)
    , distinct
    , from
    , in_
    , on
    , select
    , val
    , valList
    , where_
    , (&&.)
    , (==.)
    , (^.)
    )
import Database.Persist.Sql
    ( SqlPersistT )
import Explorer.Web.Api.Legacy.RedeemSummary
    ( queryRedeemSummary )
import Explorer.Web.Api.Legacy.Util
    ( bsBase16Encode
    , collapseTxGroup
    , decodeTextAddress
    , genesisDistributionTxHash
    , zipTxBrief
    )
import Explorer.Web.ClientTypes
    ( CAddress (..)
    , CAddressSummary (..)
    , CAddressType (..)
    , CChainTip (..)
    , CHash (..)
    , CTxAddressBrief (..)
    , CTxBrief (..)
    , CTxHash (..)
    )
import Explorer.Web.Error
    ( ExplorerError (..) )
import Explorer.Web.Query
    ( queryChainTip )

import qualified Data.List as List

-- Example redeem addresses:
--    /api/addresses/summary/Ae2tdPwUPEZAAvxJ9nQ1nx88X9Jq6dskyG9uFUWG69wC6TJ7DKNCp6QCEM2 (unspent)
--    /api/addresses/summary/Ae2tdPwUPEZKvgpCjEe7XvYMFtxYEdvEJRkY1UBkhYFVfW7sxeZtRMeax4A (unspent)
--    /api/addresses/summary/Ae2tdPwUPEZ3N2AuMYpqerifycLiLPtsV8B72VgAsPjPAwrLa7xyfWBUJ2t (spent)
--    /api/addresses/summary/Ae2tdPwUPEZHhgPpDi2g5nE1UhHn6hCqihNxwkhTgxQayQ6FwE3eKEypDZU (spent)

-- Example regular addresses:
--    /api/addresses/summary/DdzFFzCqrhsszHTvbjTmYje5hehGbadkT6WgWbaqCy5XNxNttsPNF13eAjjBHYT7JaLJz2XVxiucam1EvwBRPSTiCrT4TNCBas4hfzic
--    /api/addresses/summary/DdzFFzCqrhsq7KBnN2VngWtvPMriYFCNwWxW5PSnooLeNhNaz6Q2GhyfjHn7sY3BNiymhAkcCSTRhTBe8PZXSEEewuyWryURhmozdd6b
--    /api/addresses/summary/DdzFFzCqrht7YAnA3PW3RQEdYdQMQdodHG87H6vF766xVZuXjc3hkBN9PH8oQZGxAdQhiqgTYm24KAZyEGh3N6yxjj6Y5LfWJE83ngLp
--    /api/addresses/summary/DdzFFzCqrhsqz23SkTxevzJ3Dn4ee14BpQVe5T9LX2yWJpcjHToP2qxnzaEiy5qiHwNVtX5ANXtLJyBwKz8PvjJZYq2n8fyy7Dp9RqXa

--    /api/addresses/summary/DdzFFzCqrht1XitMM2mA1Qp4iW2jcX1Gx6f7tcJ4mfzXdS5VUW9sM28UejDGxz6eUDvdMTeuTqwZdSUwDiezcTafs89DNKEsSoCAX9ji
--    /api/addresses/summary/DdzFFzCqrhtCMkF4akdRbon1FFXStuc3YjW1gvYdE8sQKVUsgtUAwF5wf8EtSdCQ4biCRueMSDQSouqrSdhWbz2u41Xzt6WKEShgBynd

-- Binance cold storage address (huge number of inputs and outputs):
--    /api/addresses/summary/DdzFFzCqrhtBatWqyFge4w6M6VLgNUwRHiXTAg3xfQCUdTcjJxSrPHVZJBsQprUEc5pRhgMWQaGciTssoZVwrSKmG1fneZ1AeCtLgs5Y


addressSummary
    :: MonadIO m
    => CAddress
    -> SqlPersistT m (Either ExplorerError CAddressSummary)
addressSummary (CAddress addrTxt) = runExceptT $ do
    addr <- hoistEither $ decodeTextAddress addrTxt
    newExceptT $ queryAddressSummary addrTxt addr

-- -------------------------------------------------------------------------------------------------

queryAddressSummary :: MonadIO m => Text -> Address -> SqlPersistT m (Either ExplorerError CAddressSummary)
queryAddressSummary addrTxt addr = do
  chainTip <- queryChainTip
  if isRedeemAddress addr
    then queryRedeemSummary chainTip addrTxt
    else Right <$> queryNonRedeemSummary chainTip addrTxt

queryNonRedeemSummary :: MonadIO m => CChainTip -> Text -> SqlPersistT m CAddressSummary
queryNonRedeemSummary chainTip addr = do
    inrows <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
                on (tx ^. TxId ==. txOut ^. TxOutTxId)
                on (blk ^. BlockId ==. tx ^. TxBlock)
                where_ (txOut ^. TxOutAddress ==. val addr)
                pure (tx ^. TxId, tx ^. TxHash, blk ^. BlockTime)
    -- This needs to be distinct to avoid duplicate rows.
    outrows <- select . distinct . from $ \ (blk `InnerJoin` tx `InnerJoin` txIn `InnerJoin` txOut) -> do
                on (txIn ^. TxInTxOutId ==. txOut ^. TxOutTxId
                    &&. txIn ^. TxInTxOutIndex ==. txOut ^. TxOutIndex)
                on (tx ^. TxId ==. txIn ^. TxInTxInId)
                on (blk ^. BlockId ==. tx ^. TxBlock)
                where_ (txOut ^. TxOutAddress ==. val addr)
                pure (tx ^. TxId, tx ^. TxHash, blk ^. BlockTime)

    cAddressSummary
        <$> queryCTxBriefs (map unValue3 inrows)
        <*> queryCTxBriefs (map unValue3 outrows)
  where
    cAddressSummary :: [CTxBrief] -> [CTxBrief] -> CAddressSummary
    cAddressSummary itxs otxs =
      let insum = sum . map ctaAmount $ filter isTargetAddress (concatMap ctbOutputs itxs)
          outsum = sum . map ctaAmount $ filter isTargetAddress (concatMap ctbInputs otxs)
          txs = List.sortOn ctbTimeIssued (itxs ++ otxs)
          fees = sum $ map ctbFees txs
      in
      CAddressSummary
        { caAddress = CAddress addr
        , caType = CPubKeyAddress
        , caChainTip = chainTip
        , caTxNum = fromIntegral $ length txs
        , caBalance = insum - outsum
        , caTotalInput = insum
        , caTotalOutput = outsum
        , caTotalFee = fees
        , caTxList = txs
        }

    isTargetAddress :: CTxAddressBrief -> Bool
    isTargetAddress (CTxAddressBrief (CAddress tst) _ _ _) = tst == addr

-- -------------------------------------------------------------------------------------------------

queryCTxBriefs :: MonadIO m => [(TxId, ByteString, UTCTime)] -> SqlPersistT m [CTxBrief]
queryCTxBriefs [] = pure []
queryCTxBriefs xs = do
  let txids = map fst3 xs
  zipTxBrief xs <$> queryTxInputs txids <*> queryTxOutputs txids

queryTxInputs :: MonadIO m => [TxId] -> SqlPersistT m [(TxId, [CTxAddressBrief])]
queryTxInputs txids = do
    rows <- select . distinct . from $ \(tx `InnerJoin` txIn `InnerJoin` txOut `InnerJoin` txInTx) -> do
              on (txInTx ^. TxId ==. txIn ^. TxInTxOutId)
              on (txIn ^. TxInTxOutId ==. txOut ^. TxOutTxId
                  &&. txIn ^. TxInTxOutIndex ==. txOut ^. TxOutIndex)
              on (tx ^. TxId ==. txIn ^. TxInTxInId)
              where_ (txIn ^. TxInTxInId `in_` valList txids)
              pure (tx ^. TxId, txOut ^. TxOutAddress, txOut ^. TxOutValue, txInTx ^. TxHash, txOut ^. TxOutIndex, txInTx ^. TxSize ==. val 0)
    pure $ map collapseTxGroup (groupOn fst $ map convert rows)
  where
    convert :: (Value TxId, Value Text, Value Word64, Value ByteString, Value Word16, Value Bool) -> (TxId, CTxAddressBrief)
    convert (Value txid, Value addr, Value coin, Value txh, Value index, Value isGenesisTx) =
      ( txid
      , if isGenesisTx
          then
            CTxAddressBrief
              { ctaAddress = CAddress addr
              , ctaAmount = fromIntegral coin
              , ctaTxHash = genesisDistributionTxHash
              , ctaTxIndex = 0
              }
          else
            CTxAddressBrief
              { ctaAddress = CAddress addr
              , ctaAmount = fromIntegral coin
              , ctaTxHash = CTxHash $ CHash (bsBase16Encode txh)
              , ctaTxIndex = fromIntegral index
              }
      )

queryTxOutputs :: MonadIO m => [TxId] -> SqlPersistT m [(TxId, [CTxAddressBrief])]
queryTxOutputs txids = do
    rows <- select . from $ \ (tx `InnerJoin` txOut) -> do
              on (tx ^. TxId ==. txOut ^. TxOutTxId)
              where_ (tx ^. TxId `in_` valList txids)
              pure (tx ^. TxId, txOut ^. TxOutAddress, txOut ^. TxOutValue, tx ^. TxHash, txOut ^. TxOutIndex)
    pure $ map collapseTxGroup (groupOn fst $ map convert rows)
  where
    convert :: (Value TxId, Value Text, Value Word64, Value ByteString, Value Word16) -> (TxId, CTxAddressBrief)
    convert (Value txid, Value addr, Value coin, Value txhash, Value index) =
      ( txid
      , CTxAddressBrief
          { ctaAddress = CAddress addr
          , ctaAmount = fromIntegral coin
          , ctaTxHash = CTxHash . CHash $ bsBase16Encode txhash
          , ctaTxIndex = fromIntegral index
          }
      )

-- -------------------------------------------------------------------------------------------------

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
