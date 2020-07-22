{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Validate
  ( runValidation
  ) where

import Cardano.Db
    ( readPGPassFileEnv, toConnectionString )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Logger
    ( runNoLoggingT )
import Data.Text.ANSI
    ( yellow )
import Database.Persist.Postgresql
    ( withPostgresqlConn )
import Database.Persist.Sql
    ( SqlBackend )
import Explorer.Web.Api.Legacy.Util
    ( runQuery, textShow )
import Explorer.Web.Validate.Address
    ( validateAddressSummary, validateRedeemAddressSummary )
import Explorer.Web.Validate.BlocksTxs
    ( validateBlocksTxs )
import Explorer.Web.Validate.GenesisAddress
    ( validateGenesisAddressPaging )

import qualified Data.Text.IO as Text

runValidation :: Word -> IO ()
runValidation count = do
  pgconfig <- readPGPassFileEnv
  putStrLn ""
  runNoLoggingT .
    withPostgresqlConn (toConnectionString pgconfig) $ \backend ->
      liftIO $ loop backend 1
  where
    loop :: SqlBackend -> Word -> IO ()
    loop backend n
      | n > count = pure ()
      | otherwise = do
          Text.putStrLn $ yellow ("Test #" <> textShow n <> ":")
          validate backend
          loop backend (n + 1)

validate :: SqlBackend -> IO ()
validate backend = runQuery backend $ do
  validateRedeemAddressSummary
  validateAddressSummary
  validateGenesisAddressPaging
  validateBlocksTxs
  liftIO $ putStrLn ""
