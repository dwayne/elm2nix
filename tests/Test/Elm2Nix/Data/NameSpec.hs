{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.Data.NameSpec (spec) where

import qualified Data.ByteString.Lazy as LBS
import qualified Elm2Nix.Data.Bytes.Decode as BD
import qualified Elm2Nix.Data.Bytes.Encode as BE
import qualified Elm2Nix.Data.Name as Name

import Data.Foldable (traverse_)
import Test.Hspec


spec :: Spec
spec =
  describe "Elm2Nix.Data.Name" $ do
    fromTextSpec
    toTextSpec
    binarySerializationV0_19_1Spec
    binarySerializationV0_19_3Spec


fromTextSpec :: Spec
fromTextSpec =
  describe "fromText" $ do
    describe "valid input" $
      it "example 1" $
        Name.fromText "elm/core" `shouldBe` Right Name.elmCore

    describe "invalid input" $ do
      it "when author is empty" $
        let
          check t =
            Name.fromText t `shouldBe` Left Name.EmptyAuthor
        in
        traverse_ check [ "/core", " /core" ]

      it "when package is empty" $
        let
          check t =
            Name.fromText t `shouldBe` Left Name.EmptyPackage
        in
        traverse_ check [ "elm/", "elm/ " ]

      it "when / is missing" $
        Name.fromText "elmcore" `shouldBe` Left Name.MissingForwardSlash


toTextSpec :: Spec
toTextSpec =
  describe "toText" $
    it "example 1" $
      Name.toText "-" Name.elmCore `shouldBe` "elm-core"


binarySerializationV0_19_1Spec :: Spec
binarySerializationV0_19_1Spec =
  describe "binary serialization for Elm 0.19.1" $ do
    describe "encode" $
      it "example 1" $
        let
          expectedByteString =
            LBS.pack
              [ 0x03                   -- length of the UTF-8 encoding of "elm" (mod 256)
              , 0x65, 0x6C, 0x6D       -- UTF-8 encoding of "elm"
              , 0x04                   -- length of the UTF-8 encoding of "core" (mod 256)
              , 0x63, 0x6F, 0x72, 0x65 -- UTF-8 encoding of "core"
              ]
        in
        BE.encode (Name.binaryEncoderV0_19_1 Name.elmCore) `shouldBe` expectedByteString

    describe "decode" $
      it "example 1" $
        BD.decode Name.binaryDecoderV0_19_1 (LBS.pack [0x03, 0x65, 0x6C, 0x6D, 0x04, 0x68, 0x74, 0x6D, 0x6C]) `shouldBe` Right Name.elmHtml


binarySerializationV0_19_3Spec :: Spec
binarySerializationV0_19_3Spec =
  describe "binary serialization for Elm 0.19.3" $ do
    describe "encode" $
      it "example 1" $
        let
          expectedByteString =
            LBS.pack
              [ 0x03                   -- length of the UTF-8 encoding of "elm" (mod 256)
              , 0x65, 0x6C, 0x6D       -- UTF-8 encoding of "elm"
              , 0x04                   -- length of the UTF-8 encoding of "core" (mod 256)
              , 0x63, 0x6F, 0x72, 0x65 -- UTF-8 encoding of "core"
              ]
        in
        BE.encode (Name.binaryEncoderV0_19_3 Name.elmCore) `shouldBe` expectedByteString

    describe "decode" $
      it "example 1" $
        BD.decode Name.binaryDecoderV0_19_3 (LBS.pack [0x03, 0x65, 0x6C, 0x6D, 0x04, 0x68, 0x74, 0x6D, 0x6C]) `shouldBe` Right Name.elmHtml
