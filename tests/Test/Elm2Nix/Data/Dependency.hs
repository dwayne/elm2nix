{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.Data.Dependency (main) where

import Test.Hspec

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.Version (Version(..))


main :: IO ()
main = hspec $
    describe "Elm2Nix.Data.Dependency" orderSpec


orderSpec :: Spec
orderSpec =
    describe "order" $ do
        it "elm-core-1.0.5 < elm-json-1.1.3" $
            Dependency "elm" "core" (Version 1 0 5) < Dependency "elm" "json" (Version 1 1 3)

        it "elm-browser-1.0.2 > elm-browser-1.0.1" $
            Dependency "elm" "browser" (Version 1 0 2) > Dependency "elm" "browser" (Version 1 0 1)
