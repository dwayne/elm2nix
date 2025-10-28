module Test.Elm2Nix.Version (main) where

import Test.Hspec

import Elm2Nix.Version (Version(..))


main :: IO ()
main = hspec $
    describe "Elm2Nix.Version" $ do
        showSpec
        orderSpec


showSpec :: Spec
showSpec =
    describe "show" $
        it "uses the major.minor.patch format" $
            show (Version 1 2 3) == "1.2.3"


orderSpec :: Spec
orderSpec =
    describe "order" $
        it "100.0.0 > 2.0.0" $
            --
            -- N.B. "100.0.0" < "2.0.0"
            --
            Version 100 0 0 > Version 2 0 0
