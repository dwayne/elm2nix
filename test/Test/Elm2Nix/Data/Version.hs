{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.Data.Version (main) where

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS

import Control.Exception (evaluate)
import Test.Hspec

import qualified Elm2Nix.Data.Version as Version
import qualified Elm2Nix.Lib.Json.Decode as JD

import Elm2Nix.Data.Version (Version(..))


main :: IO ()
main = hspec $
    describe "Elm2Nix.Data.Version" $ do
        fromTextSpec
        decoderSpec
        orderSpec
        showSpec
        binarySerializationSpec


fromTextSpec :: Spec
fromTextSpec =
    describe "fromText" $ do
        describe "valid input" $
            it "example 1" $
                Version.fromText "0.1.65535" `shouldBe` Just (Version 0 1 65535)

        describe "invalid input" $ do
            it "must be of the form X.Y.Z" $
                Version.fromText "1.2" `shouldBe` Nothing

            it "must not have a part with leading zeros" $
                Version.fromText "1.02.3" `shouldBe` Nothing

            it "must have each part fit in a 16-bit unsigned integer" $
                Version.fromText "1.2.65536" `shouldBe` Nothing


decoderSpec :: Spec
decoderSpec =
    describe "decoder" $ do
        it "example 1" $
            JD.decodeString Version.decoder "\"1.2.3\"" `shouldBe` Right (Version 1 2 3)

        it "example 2" $
            JD.decodeString Version.decoder "\"1.2\"" `shouldBe` Left (JD.DecodeError (JD.Failure "version is invalid: 1.2"))


orderSpec :: Spec
orderSpec =
    describe "order" $
        it "100.0.0 > 2.0.0" $
            --
            -- N.B. As strings "100.0.0" < "2.0.0".
            --
            Version 100 0 0 > Version 2 0 0


showSpec :: Spec
showSpec =
    describe "show" $
        it "example 1" $
            show (Version 1 2 3) `shouldBe` "1.2.3"


binarySerializationSpec :: Spec
binarySerializationSpec =
    describe "binary serialization" $ do
        describe "encode" $ do
            describe "when major, minor, and patch are all less than 256" $
                it "encodes using 8-bits each" $
                    Binary.encode (Version 1 2 3) `shouldBe` LBS.pack [0x01, 0x02, 0x03]

            describe "when major is 256 or more" $
                it "encodes using a 255 tag followed by 16-bits each" $
                    Binary.encode (Version 256 2 3) `shouldBe` LBS.pack [0xFF, 0x01, 0x00, 0x00, 0x02, 0x00, 0x03]

        describe "decode" $ do
            it "example 1" $
                Binary.decode (LBS.pack [0x01, 0x00, 0x05]) `shouldBe` Version 1 0 5

            it "example 2" $
                Binary.decode (LBS.pack [0xFF, 0x01, 0x01, 0x00, 0x00, 0xFF, 0xFF]) `shouldBe` Version 257 0 65535

        describe "when major is 255" $
            it "does the wrong thing" $
                --
                -- A possible bug.
                --
                -- This happens because 255 is used to determine if the parts were each encoded using 8-bits or 16-bits.
                --
                -- When major is 255 the decoder expects to see 7 bytes but only 3 bytes were encoded since each part
                -- was less than 256.
                --
                -- I think a simple fix is to use major < 255 && minor < 255 && patch < 255.
                --
                -- Even the following major < 255 && minor < 256 && patch < 256 works. We just can't have major = 255
                -- when encoding each part using 8-bits.
                --
                evaluate (Binary.decode $ Binary.encode $ Version 255 2 3 :: Version) `shouldThrow` anyErrorCall
