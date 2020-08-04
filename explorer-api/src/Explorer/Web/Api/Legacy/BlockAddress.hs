{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Api.Legacy.BlockAddress
  ( blockAddress
  ) where

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
import Data.Maybe
    ( listToMaybe )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( UTCTime )
import Data.Time.Clock.POSIX
    ( utcTimeToPOSIXSeconds )
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
    , (<=.)
    , (==.)
    , (^.)
    )
import Database.Persist.Sql
    ( SqlPersistT )
import Explorer.Web.Api.Legacy.Util
    ( bsBase16Encode
    , collapseTxGroup
    , decodeTextAddress
    , isRedeemAddress
    , textBase16Decode
    , zipTxBrief
    )
import Explorer.Web.ClientTypes
    ( CAddress (..)
    , CAddressSummary (..)
    , CAddressType (..)
    , CChainTip (..)
    , CCoin (..)
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

-- This is a new endpoint that was requested by one of the large exchanges.
-- It did not exist in the original legacy explorer webapi.
--
-- It is very similar to the '/api/addresses/summary/{address}' but requires the
-- block hash as well and retrieves data about the address at (eg after) that block
-- has been applied. Like the other query, the performance can be poor for adddresses
-- with large numbers of transactions (eg 1000 or more).

-- Example call:
--  /api/block/{blkHash}/address/{address}

blockAddress
    :: MonadIO m
    => CHash -> CAddress
    -> SqlPersistT m (Either ExplorerError CAddressSummary)
blockAddress (CHash blkHashTxt) (CAddress addrTxt) =
  runExceptT $ do
    addr <- hoistEither $ decodeTextAddress addrTxt
    blkHash <- hoistEither $ textBase16Decode blkHashTxt
    newExceptT $ do
        chainTip <- queryChainTip
        if isRedeemAddress addr
          then queryRedeemSummary chainTip blkHash addrTxt
          else Right <$> queryAddressSummary chainTip blkHash addrTxt

-- -------------------------------------------------------------------------------------------------

queryRedeemSummary :: MonadIO m => CChainTip -> ByteString -> Text -> SqlPersistT m (Either ExplorerError CAddressSummary)
queryRedeemSummary chainTip blkHash addrTxt = do
    -- Find the initial value assigned to this address at Genesis
    rows <- select . from $ \ txOut -> do
              where_ (txOut ^. TxOutAddress ==. val addrTxt)
              pure (txOut ^. TxOutValue)
    case rows of
      [] -> pure $ Left (Internal "queryRedeemSummary: Address not found")
      [value] -> Right <$> queryRedeemed (fromIntegral $ unValue value)
      _ -> pure $ Left (Internal "queryRedeemSummary: More than one entry")
  where
    queryRedeemed :: MonadIO m => CCoin -> SqlPersistT m CAddressSummary
    queryRedeemed value = do
      -- Query to see if the Genesis value has been spent.
      -- Will return [] if unspent and otherwise a single row.
      outrows <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txIn `InnerJoin` txOut0 `InnerJoin` txOut1) -> do
                    on (tx ^. TxId ==. txOut1 ^. TxOutTxId)
                    on (txIn ^. TxInTxOutId ==. txOut0 ^. TxOutTxId
                        &&. txIn ^. TxInTxOutIndex ==. txOut0 ^. TxOutIndex)
                    on (tx ^. TxId ==. txIn ^. TxInTxInId)
                    on (blk ^. BlockId ==. tx ^. TxBlock)
                    where_ (blk ^. BlockHash <=. val blkHash)
                    where_ (txOut0 ^. TxOutAddress ==. val addrTxt)
                    pure (tx ^. TxHash, blk ^. BlockTime, txOut1 ^. TxOutAddress)
      pure $ maybe (convertUnspent value) (convertSpent value) (unValue3 <$> listToMaybe outrows)

    convertUnspent :: CCoin -> CAddressSummary
    convertUnspent balance =
      CAddressSummary
        { caAddress = CAddress addrTxt
        , caType = CRedeemAddress
        , caChainTip = chainTip
        , caTxNum = 0
        , caBalance = balance
        , caTotalInput = 0
        , caTotalOutput = 0
        , caTotalFee = 0
        , caTxList = []
        }

    convertSpent :: CCoin -> (ByteString, UTCTime, Text) -> CAddressSummary
    convertSpent outval (txhash, utctime, outAddr) =
      CAddressSummary
        { caAddress = CAddress addrTxt
        , caType = CRedeemAddress
        , caChainTip = chainTip
        , caTxNum = 1
        , caBalance = 0
        , caTotalInput = outval
        , caTotalOutput = outval
        , caTotalFee = 0
        , caTxList =
            [ CTxBrief
                { ctbId = CTxHash . CHash $ bsBase16Encode txhash
                , ctbTimeIssued = Just $ utcTimeToPOSIXSeconds utctime
                , ctbInputs =
                    [ CTxAddressBrief
                        { ctaAddress = CAddress outAddr
                        , ctaAmount = outval
                        , ctaTxHash = CTxHash . CHash $ bsBase16Encode txhash
                        , ctaTxIndex = 0
                        }
                    ]
                , ctbOutputs =
                    [ CTxAddressBrief
                        { ctaAddress = CAddress outAddr
                        , ctaAmount = outval
                        , ctaTxHash = CTxHash . CHash $ bsBase16Encode txhash
                        , ctaTxIndex = 0
                        }
                    ]
                , ctbInputSum = outval
                , ctbOutputSum = outval
                , ctbFees = 0
                }
            ]
        }

-- -------------------------------------------------------------------------------------------------

queryAddressSummary :: MonadIO m => CChainTip -> ByteString -> Text -> SqlPersistT m CAddressSummary
queryAddressSummary chainTip blkHash addr = do
    inrows <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
                on (tx ^. TxId ==. txOut ^. TxOutTxId)
                on (blk ^. BlockId ==. tx ^. TxBlock)
                where_ (blk ^. BlockHash <=. val blkHash)
                where_ (txOut ^. TxOutAddress ==. val addr)
                pure (tx ^. TxId, tx ^. TxHash, blk ^. BlockTime)
    -- This needs to be distinct to avoid duplicate rows.
    outrows <- select . distinct . from $ \ (blk `InnerJoin` tx `InnerJoin` txIn `InnerJoin` txOut) -> do
                on (txIn ^. TxInTxOutId ==. txOut ^. TxOutTxId
                    &&. txIn ^. TxInTxOutIndex ==. txOut ^. TxOutIndex)
                on (tx ^. TxId ==. txIn ^. TxInTxInId)
                on (blk ^. BlockId ==. tx ^. TxBlock)
                where_ (blk ^. BlockHash <=. val blkHash)
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
  rows <- select . distinct . from $ \(tx `InnerJoin` txIn `InnerJoin` txOut) -> do
            on (txIn ^. TxInTxOutId ==. txOut ^. TxOutTxId
                &&. txIn ^. TxInTxOutIndex ==. txOut ^. TxOutIndex)
            on (tx ^. TxId ==. txIn ^. TxInTxInId)
            where_ (txIn ^. TxInTxInId `in_` valList txids)
            pure (tx ^. TxId, txOut ^. TxOutAddress, txOut ^. TxOutValue, tx ^. TxHash, txOut ^. TxOutIndex)
  pure $ map collapseTxGroup (groupOn fst $ map convert rows)

queryTxOutputs :: MonadIO m => [TxId] -> SqlPersistT m [(TxId, [CTxAddressBrief])]
queryTxOutputs txids = do
  rows <- select . from $ \ (tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            where_ (tx ^. TxId `in_` valList txids)
            pure (tx ^. TxId, txOut ^. TxOutAddress, txOut ^. TxOutValue, tx ^. TxHash, txOut ^. TxOutIndex)
  pure $ map collapseTxGroup (groupOn fst $ map convert rows)

-- -------------------------------------------------------------------------------------------------

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

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
