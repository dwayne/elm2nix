{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.Data.Version (main) where

import Test.Hspec

import qualified Elm2Nix.Data.Version as Version

import Elm2Nix.Data.Version (Version(..))


main :: IO ()
main = hspec $
    describe "Elm2Nix.Data.Version" $ do
        fromTextSpec
        orderSpec
        showSpec


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
