{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.Data.Name (main) where

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS

import Test.Hspec

import qualified Elm2Nix.Data.Name as Name


main :: IO ()
main = hspec $
    describe "Elm2Nix.Data.Name" $ do
        fromTextSpec
        toTextSpec
        binarySerializationSpec


fromTextSpec :: Spec
fromTextSpec =
    describe "fromText" $ do
        describe "valid input" $
            it "example 1" $
                Name.fromText "elm/core" `shouldBe` Right Name.elmCore

        describe "invalid input" $ do
            it "when author is empty" $
                Name.fromText "/core" `shouldBe` Left Name.EmptyAuthor

            it "when package is empty" $
                Name.fromText "elm/" `shouldBe` Left Name.EmptyPackage

            it "when / is missing" $
                Name.fromText "elmcore" `shouldBe` Left Name.MissingForwardSlash


toTextSpec :: Spec
toTextSpec =
    describe "toText" $
        it "example 1" $
            Name.toText "-" Name.elmCore `shouldBe` "elm-core"


binarySerializationSpec :: Spec
binarySerializationSpec =
    describe "binary serialization" $ do
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
                Binary.encode Name.elmCore `shouldBe` expectedByteString

        describe "decode" $
            it "example 1" $
                Binary.decode (LBS.pack [0x03, 0x65, 0x6C, 0x6D, 0x04, 0x68, 0x74, 0x6D, 0x6C]) `shouldBe` Name.elmHtml
