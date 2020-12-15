{-# LANGUAGE OverloadedStrings #-}

module Test.Explorer.Web.Api.Legacy.UtilSpec
    ( spec
    ) where

import Prelude
import Cardano.Api.MetaData (TxMetadataValue (..))

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Either
    ( isLeft, isRight )
import qualified Data.Aeson as Aeson
import Explorer.Web.Api.Legacy.Util
    ( decodeTextAddress, jsonToMetadataValue)
import Test.Hspec
    ( Spec, describe, it, shouldSatisfy, shouldBe)

spec :: Spec
spec = do
    describe "decodeTextAddress" $ do
        it "x cannot decode arbitrary unicode" $ do
            decodeTextAddress
                "💩"
                `shouldSatisfy` isLeft

        it "x cannot decode gibberish base58" $ do
            decodeTextAddress
                "FpqXJLkgVhRTYkf5F7mh3q6bAv5hWYjhSV1gekjEJE8XFeZSganv"
                `shouldSatisfy` isLeft

        it "x cannot decode Jörmungandr address" $ do
            decodeTextAddress
                "addr1qdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677z5t3m7d"
                `shouldSatisfy` isLeft

        it "✓ can decode Base58 Byron address (Legacy Mainnet)" $ do
            decodeTextAddress
                "DdzFFzCqrhstkaXBhux3ALL9wqvP3Nkz8QE5qKwFbqkmTL6zyKpc\
                \FpqXJLkgVhRTYkf5F7mh3q6bAv5hWYjhSV1gekjEJE8XFeZSganv"
                `shouldSatisfy` isRight

        it "✓ can decode Base58 Byron address (Legacy Testnet)" $ do
            decodeTextAddress
                "37btjrVyb4KEgoGCHJ7XFaJRLBRiVuvcrQWPpp4HeaxdTxhKwQjXHNKL4\
                \3NhXaQNa862BmxSFXZFKqPqbxRc3kCUeTRMwjJevFeCKokBG7A7num5Wh"
                `shouldSatisfy` isRight

        it "✓ can decode Base58 Byron address (Icarus)" $ do
            decodeTextAddress
                "Ae2tdPwUPEZ4Gs4s2recjNjQHBKfuBTkeuqbHJJrC6CuyjGyUD44cCTq4sJ"
                `shouldSatisfy` isRight

        it "✓ can decode Bech32 Shelley address (basic)" $ do
            decodeTextAddress
                "addr1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0yu80w"
                `shouldSatisfy` isRight

        it "✓ can decode Bech32 Shelley address (stake by value)" $ do
            decodeTextAddress
                "addr1qrejymax5dh6srcj3sehf6lt0czdj9uklffzhc3fqgglnl\
                \0nyfh6dgm04q839rpnwn47klsymyted7jj903zjqs3l87sutspf7"
                `shouldSatisfy` isRight

        it "✓ can decode Bech32 Shelley address (pointer)" $ do
            decodeTextAddress
                "addr1g8ejymax5dh6srcj3sehf6lt0czdj9uklffzhc3fqgglnlf2pcqqc69etp"
                `shouldSatisfy` isRight

        it "✓ can decode Base16 Byron address" $ do
            decodeTextAddress
                "82d818582183581c4ad651d1c6afe6b3483eac69ab9\
                \6575519376f136f024c35e5d27071a0001a453d0d11"
                `shouldSatisfy` isRight

        it "✓ can decode Base16 Shelley address" $ do
            decodeTextAddress
                "6079467c69a9ac66280174d09d62575ba955748b21dec3b483a9469a65"
                `shouldSatisfy` isRight

        it "✓ can parse tx text JSON metadata" $ do
            jsonToMetadataValue
                (Aeson.String "foo")
                `shouldBe` Just (TxMetaText "foo")
        
        it "✓ can parse tx encoded binary JSON metadata" $ do
            jsonToMetadataValue
                (Aeson.String "0x666f6f")
                `shouldBe` Just (TxMetaBytes "foo")

        it "✓ can parse tx integer JSON metadata" $ do
            jsonToMetadataValue
                (Aeson.Number 12345678910)
                `shouldBe` Just (TxMetaNumber 12345678910)

        it "✓ can parse tx list JSON metadata" $ do
            jsonToMetadataValue (
                Aeson.Array (V.fromList [
                    Aeson.Number 1,
                    Aeson.String "foo",
                    Aeson.Bool True
                ]))
                `shouldBe` (Just (TxMetaList [
                    TxMetaNumber 1,
                    TxMetaText "foo"
                ]))
                        
        it "✓ can parse tx map JSON metadata" $ do
            jsonToMetadataValue ( Aeson.Object $
                HM.fromList [
                    ("somekey", Aeson.Number 1),
                    ("1", Aeson.Number 2),
                    ("0x666f6f", Aeson.String "bar")
                ])
                `shouldBe` (Just (TxMetaMap [
                    (TxMetaText "somekey", TxMetaNumber 1),
                    (TxMetaText "1", TxMetaNumber 2),
                    (TxMetaBytes "foo", TxMetaText "bar")
                ]))
