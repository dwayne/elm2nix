{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.Data.Name (main) where

import Test.Hspec

import qualified Elm2Nix.Data.Name as Name


main :: IO ()
main = hspec $
    describe "Elm2Nix.Data.Name" $ do
        fromTextSpec
        toTextSpec


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
