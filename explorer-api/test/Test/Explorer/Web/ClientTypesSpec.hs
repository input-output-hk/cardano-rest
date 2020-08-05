{-# LANGUAGE OverloadedStrings #-}

module Test.Explorer.Web.ClientTypesSpec
  ( spec
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
    ( Gen, PropertyT )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_roundtrip_ada_to_ccoin :: PropertyT IO ()
prop_roundtrip_ada_to_ccoin = do
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

spec :: Spec
spec = describe "ClientTypesSpec" $ do
    it "properties" $ do
        result <- H.checkParallel $ H.Group "ClientTypesSpec"
            [ ( "adaToCCoin . cCoinToAda"
              , H.property prop_roundtrip_ada_to_ccoin
              )
            ]
        result `shouldBe` True
