{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Explorer.Web.Validate.Random
  ( queryRandomAddress
  , queryRandomBlockHash
  , queryRandomRedeemAddress
  ) where

import Cardano.Db
    ( BlockId, EntityField (..), Key (..), LookupFail (..), maybeToEither )
import Control.Monad.IO.Class
    ( MonadIO )
import Control.Monad.Trans.Reader
    ( ReaderT )
import Data.ByteString.Char8
    ( ByteString )
import Data.Maybe
    ( listToMaybe )
import Data.Text
    ( Text )
import Database.Esqueleto
    ( InnerJoin (..)
    , SqlBackend
    , SqlExpr
    , SqlQuery
    , Value (..)
    , asc
    , from
    , limit
    , on
    , orderBy
    , select
    , val
    , where_
    , (==.)
    , (>.)
    , (^.)
    )
import Database.Esqueleto.PostgreSQL
    ( random_ )

-- | Get a random address.
queryRandomAddress :: MonadIO m => ReaderT SqlBackend m (Either LookupFail Text)
queryRandomAddress = do
    res <- select . from $ \ txOut -> do
             firstRandomRow
             pure (txOut ^. TxOutAddress)
    pure $ maybeToEither errMsg unValue (listToMaybe res)
  where
    errMsg :: LookupFail
    errMsg = DbLookupMessage "queryRandomAddress: Lookup address"

queryRandomBlockHash :: MonadIO m => ReaderT SqlBackend m (Either LookupFail ByteString)
queryRandomBlockHash = do
    res <- select . from $ \ blk -> do
             where_ (blk ^. BlockTxCount >. val 0)
             firstRandomRow
             limit 1
             pure (blk ^. BlockHash)
    pure $ maybeToEither errMsg unValue (listToMaybe res)
  where
    errMsg :: LookupFail
    errMsg = DbLookupMessage "queryRandomBlockHash: Lookup block by index failed"

queryRandomRedeemAddress :: MonadIO m => ReaderT SqlBackend m (Either LookupFail Text)
queryRandomRedeemAddress = do
    res <- select . from $ \ (tx `InnerJoin` txOut) -> do
             -- Block 1 contains all the TxOuts from the Genesis Distribution.
             where_ (tx ^. TxBlock ==. val (mkBlockId 1))
             on (tx ^. TxId ==. txOut ^. TxOutTxId)
             firstRandomRow
             pure (txOut ^. TxOutAddress)
    pure $ maybeToEither errMsg unValue (listToMaybe res)
  where
    errMsg ::  LookupFail
    errMsg = DbLookupMessage "queryRandomRedeemAddress: Lookup address"

mkBlockId :: Word -> BlockId
mkBlockId = BlockKey . fromIntegral

-- | Filter a table to pick a row at random row from the database.
--
-- Note: This will be fine for ad-hoc queries, but if you use it for
-- anything high-performance, you should test it thoroughly first and
-- consider alternative approaches.
firstRandomRow :: SqlQuery ()
firstRandomRow = do
  orderBy [asc (random_ :: SqlExpr (Value Int))]
  limit 1
