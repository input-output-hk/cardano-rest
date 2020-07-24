{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Validate.Address
  ( validateAddressSummary
  , validateRedeemAddressSummary
  ) where

import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.Text
    ( Text )
import qualified Data.Text as Text
import Data.Text.ANSI
    ( green, red )
import qualified Data.Text.IO as Text

import Database.Persist.Sql
    ( SqlPersistT )

import Explorer.Web
    ( CAddress (..)
    , CAddressSummary (..)
    , CHash (..)
    , CTxBrief (..)
    , CTxHash (..)
    , queryAddressSummary
    , unCCoin
    )
import Explorer.Web.Api.Legacy.Util
    ( decodeTextAddress, textShow )
import Explorer.Web.Validate.ErrorHandling
    ( handleExplorerError, handleLookupFail )
import Explorer.Web.Validate.Random
    ( queryRandomAddress, queryRandomRedeemAddress )

import System.Exit
    ( exitFailure )

-- | Validate that all address have a balance >= 0.
validateAddressSummary :: MonadIO m => SqlPersistT m ()
validateAddressSummary = do
  addrSum <- do
    addrTxt <- handleLookupFail =<< queryRandomAddress
    addr <- handleExplorerError $ decodeTextAddress addrTxt
    handleExplorerError =<< queryAddressSummary addrTxt addr
  liftIO $ do
    if caBalance addrSum >= 0
    then reportAddressSummaryOk addrSum
    else reportAddressSummaryFail addrSum
    validateAddressTotalFees addrSum
    validateTxFeeNonNegative addrSum

-- | Validate that all redeem address have a balance >= 0.
validateRedeemAddressSummary :: MonadIO m => SqlPersistT m ()
validateRedeemAddressSummary = do
  addrSum <- do
    addrTxt <- handleLookupFail =<< queryRandomRedeemAddress
    addr <- handleExplorerError $ decodeTextAddress addrTxt
    handleExplorerError =<< queryAddressSummary addrTxt addr
  liftIO $ if caBalance addrSum >= 0
    then reportAddressSummaryOk addrSum
    else reportAddressSummaryFail addrSum

-- -------------------------------------------------------------------------------------------------

validateAddressTotalFees :: CAddressSummary -> IO ()
validateAddressTotalFees addrSum =
    if caBalance addrSum >= 0
      then reportAddressFeesOk
      else reportAddressFeesFail
  where
    reportAddressFeesOk :: IO ()
    reportAddressFeesOk = do
      mapM_ Text.putStr
        [ "  Total fees for address " , shortenAddress (unCAddress $ caAddress addrSum)
        , " is non-negative: "
        ]
      Text.putStrLn $ green "ok"

    reportAddressFeesFail :: IO ()
    reportAddressFeesFail = do
      Text.putStrLn $ "  Total fees for address are negative: "
      reportCAddressSummary addrSum
      exitFailure

validateTxFeeNonNegative :: CAddressSummary -> IO ()
validateTxFeeNonNegative addrSum =
    case filter (\x -> ctbFees x < 0) (caTxList addrSum) of
      [] -> reportAddressSummaryTxFeeOk
      xs -> reportAddressSummaryTxFeeFail xs
  where
    reportAddressSummaryTxFeeOk :: IO ()
    reportAddressSummaryTxFeeOk = do
      mapM_ Text.putStr
        [ "  Individual tx fees for address " , shortenAddress (unCAddress $ caAddress addrSum)
        , " is non-negative: "
        ]
      Text.putStrLn $ green "ok"

    reportAddressSummaryTxFeeFail :: [CTxBrief] -> IO ()
    reportAddressSummaryTxFeeFail xs = do
      mapM_ Text.putStr
        [ "  Individual tx fees for address " , shortenAddress (unCAddress $ caAddress addrSum)
        , " are negative: "
        ]
      mapM_ reportCTxBrief xs
      exitFailure

-- -------------------------------------------------------------------------------------------------

reportAddressSummaryOk :: CAddressSummary -> IO ()
reportAddressSummaryOk addrSum = do
  mapM_ Text.putStr
    [ "  Balance for " , shortenAddress (unCAddress $ caAddress addrSum)
    , " (", textShow (caType addrSum), ") is non-negative: "
    ]
  Text.putStrLn $ green "ok"

reportAddressSummaryFail :: CAddressSummary -> IO ()
reportAddressSummaryFail addrSum = do
  Text.putStrLn $ "  Address balance is negative: "
  reportCAddressSummary addrSum
  exitFailure

reportCAddressSummary :: CAddressSummary -> IO ()
reportCAddressSummary addrSum =
  mapM_ Text.putStrLn
    [ "  Address: " <> unCAddress (caAddress addrSum)
    , "    type: " <> textShow (caType addrSum)
    , "    tx count: " <> textShow (caTxNum addrSum)
    , "    balance: " <>
            let balance = caBalance addrSum in
            (if balance < 0
               then red
               else green) $
              textShow $ unCCoin balance
    , "    fees: " <>
            let fees = caTotalFee addrSum in
            (if fees < 0
               then red
               else green) $
              textShow $ unCCoin fees
    , ""
    ]

reportCTxBrief :: CTxBrief -> IO ()
reportCTxBrief tx =
  mapM_ Text.putStrLn
    [ "  Tx: " <> unCTxHash (ctbId tx)
    , "    input count: " <> textShow (length $ ctbInputs tx)
    , "    output count: " <> textShow (length $ ctbOutputs tx)
    , "    fees: " <>
            let fees = ctbFees tx in
            (if fees < 0
               then red
               else green) $
              textShow $ unCCoin fees
    , ""
    ]

shortenAddress :: Text -> Text
shortenAddress addr =
  mconcat [Text.take 10 addr, "...", Text.drop (Text.length addr - 10) addr]

unCTxHash :: CTxHash -> Text
unCTxHash (CTxHash (CHash txt)) = txt
