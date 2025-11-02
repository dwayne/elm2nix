{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.Data.Name (main) where

import Test.Hspec

import qualified Elm2Nix.Data.Name as Name


main :: IO ()
main = hspec $
    describe "Elm2Nix.Data.Name" $ do
        fromTextSpec
        toStringSpec


fromTextSpec :: Spec
fromTextSpec =
    describe "fromText" $ do
        it "requires a non-empty author" $
            Name.fromText "/core" `shouldBe` Left "author is empty"

        it "requires a non-empty package" $
            Name.fromText "elm/" `shouldBe` Left "package is empty"

        it "requires a /" $
            Name.fromText "elmcore" `shouldBe` Left "/ is missing"

        it "returns a Name when the input is valid" $
            show <$> Name.fromText "elm/core" `shouldBe` Right "elm/core"


toStringSpec :: Spec
toStringSpec =
    describe "toString" $
        it "combines author and package with the separator" $
            Name.toString "-" Name.elmCore `shouldBe` "elm-core"
