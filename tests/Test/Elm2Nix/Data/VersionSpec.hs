{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.Data.VersionSpec (spec) where

import qualified Data.ByteString.Lazy as LBS
import qualified Elm2Nix.Data.Bytes.Decode as BD
import qualified Elm2Nix.Data.Bytes.Encode as BE
import qualified Elm2Nix.Data.Version as Version
import qualified Json.Decode as JD

import Elm2Nix.Data.Version (Version(..))
import Test.Hspec


spec :: Spec
spec =
  describe "Elm2Nix.Data.Version" $ do
    fromTextSpec
    orderSpec
    showSpec
    jsonDecoderSpec
    binarySerializationV0_19_1Spec
    binarySerializationV0_19_3Spec


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


jsonDecoderSpec :: Spec
jsonDecoderSpec =
  describe "jsonDecoder" $ do
    it "example 1" $
      JD.decodeText Version.jsonDecoder "\"1.2.3\"" `shouldBe` Right (Version 1 2 3)

    it "example 2" $
      JD.decodeText Version.jsonDecoder "\"1.2\"" `shouldSatisfy`
        \case
          Left (JD.DecodeError (JD.Failure "version is invalid: 1.2" _)) ->
            True

          _ ->
            False


binarySerializationV0_19_1Spec :: Spec
binarySerializationV0_19_1Spec =
  describe "binary serialization for Elm 0.19.1" $ do
    describe "encode" $ do
      describe "when major, minor, and patch are all less than 256" $
        it "encodes using 8-bits each" $
          BE.encode (Version.binaryEncoderV0_19_1 $ Version 1 2 3) `shouldBe` LBS.pack [0x01, 0x02, 0x03]

      describe "when major is 256 or more" $
        it "encodes using a 255 tag followed by 16-bits each" $
          BE.encode (Version.binaryEncoderV0_19_1 $ Version 256 2 3) `shouldBe` LBS.pack [0xFF, 0x01, 0x00, 0x00, 0x02, 0x00, 0x03]

    describe "decode" $ do
      it "example 1" $
        BD.decode Version.binaryDecoderV0_19_1 (LBS.pack [0x01, 0x00, 0x05]) `shouldBe` Right (Version 1 0 5)

      it "example 2" $
        BD.decode Version.binaryDecoderV0_19_1 (LBS.pack [0xFF, 0x01, 0x01, 0x00, 0x00, 0xFF, 0xFF]) `shouldBe` Right (Version 257 0 65535)

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
        BD.decode Version.binaryDecoderV0_19_1 (BE.encode (Version.binaryEncoderV0_19_1 $ Version 255 2 3))
        `shouldBe`
        Left (BD.Failure LBS.empty 3 "not enough bytes")


binarySerializationV0_19_3Spec :: Spec
binarySerializationV0_19_3Spec =
  describe "binary serialization for Elm 0.19.3" $ do
    describe "encode" $ do
      describe "when major, minor, and patch are all less than 256" $
        it "encodes using 8-bits each" $
          BE.encode (Version.binaryEncoderV0_19_3 $ Version 1 2 3) `shouldBe` LBS.pack [0x01, 0x02, 0x03]

      describe "when major is 256 or more" $
        it "encodes using a 255 tag followed by 16-bits each" $
          BE.encode (Version.binaryEncoderV0_19_3 $ Version 256 2 3) `shouldBe` LBS.pack [0xFF, 0x00, 0x01, 0x02, 0x00, 0x03, 0x00]

    describe "decode" $ do
      it "example 1" $
        BD.decode Version.binaryDecoderV0_19_3 (LBS.pack [0x01, 0x00, 0x05]) `shouldBe` Right (Version 1 0 5)

      it "example 2" $
        BD.decode Version.binaryDecoderV0_19_3 (LBS.pack [0xFF, 0x01, 0x01, 0x00, 0x00, 0xFF, 0xFF]) `shouldBe` Right (Version 257 0 65535)

    describe "when major is 255" $ do
      it "example 1" $
        BE.encode (Version.binaryEncoderV0_19_3 $ Version 255 2 3) `shouldBe` LBS.pack [0xFF, 0xFF, 0x00, 0x02, 0x00, 0x03, 0x00]

      it "example 2" $
        BD.decode Version.binaryDecoderV0_19_3 (LBS.pack [0xFF, 0xFF, 0x00, 0x02, 0x00, 0x03, 0x00]) `shouldBe` Right (Version 255 2 3)
