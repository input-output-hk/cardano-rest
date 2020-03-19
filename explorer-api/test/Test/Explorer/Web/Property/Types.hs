{-# LANGUAGE TemplateHaskell #-}

module Test.Explorer.Web.Property.Types
  ( tests
  ) where

import Cardano.Chain.Common
    ( maxLovelaceVal )
import Cardano.Db
    ( Ada (..), word64ToAda )
import Data.Word
    ( Word64 )
import Explorer.Web.ClientTypes
    ( adaToCCoin, cCoinToAda )
import Hedgehog
    ( Gen, Property, discover )

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_roundtrip_ada_to_ccoin :: Property
prop_roundtrip_ada_to_ccoin =
  H.withTests 1000 . H.property $ do
    ada <- H.forAll genAda
    H.tripping ada adaToCCoin (Just . cCoinToAda)

genAda :: Gen Ada
genAda =
    word64ToAda <$> genWord64Ada
  where
    genWord64Ada :: Gen Word64
    genWord64Ada =
      Gen.choice
        [ Gen.word64 (Range.linear 0 maxLovelaceVal) -- Full range
        , Gen.word64 (Range.linear 0 5000)           -- Small values
        , Gen.word64 (Range.linear (maxLovelaceVal - 5000) maxLovelaceVal) -- Near max.
        ]

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$discover
