{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.Data.Dependency (main) where

import Test.Hspec

import qualified Elm2Nix.Data.Dependency as Dependency
import qualified Elm2Nix.Data.Name as Name

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.Version (Version(..))


main :: IO ()
main = hspec $
    describe "Elm2Nix.Data.Dependency" $ do
        orderSpec
        showSpec
        toUrlSpec


orderSpec :: Spec
orderSpec =
    describe "order" $ do
        it "elm-core-2.3.4 < elm-json-1.2.3" $
            Dependency Name.elmCore (Version 2 3 4) < Dependency Name.elmJson (Version 1 2 3)

        it "elm-browser-1.0.2 > elm-browser-1.0.1" $
            Dependency Name.elmBrowser (Version 1 0 2) > Dependency Name.elmBrowser (Version 1 0 1)


showSpec :: Spec
showSpec =
    describe "show" $
        it "example 1" $
            show (Dependency Name.elmCore (Version 1 0 5)) `shouldBe` "elm-core-1.0.5"


toUrlSpec :: Spec
toUrlSpec =
    describe "toUrl" $
        it "example 1" $
            Dependency.toUrl (Dependency Name.elmCore (Version 1 0 5)) `shouldBe` "https://github.com/elm/core/archive/1.0.5.tar.gz"
