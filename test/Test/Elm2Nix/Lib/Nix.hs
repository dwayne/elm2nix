module Test.Elm2Nix.Lib.Nix (main) where

import Test.Hspec

import Elm2Nix.Lib.Nix (NixPrefetchUrlError(..), NixPrefetchUrlOutput(..), Url, nixPrefetchUrl)


main :: IO ()
main = hspec $
    describe "Elm2Nix.Lib.Nix" nixPrefetchUrlSpec


nixPrefetchUrlSpec :: Spec
nixPrefetchUrlSpec =
    describe "nixPrefetchUrl (network)" $ do
        describe "valid input" $
            it "example 1" $
                let
                    output = NixPrefetchUrlOutput hash path
                    hash = "0g3xbi8f9k5q45s95nx3jfvzwdf4b2n63a52wr4027d2xjx0pmvl"
                    path = "/nix/store/y1h9ay7hd8sij79sd41344gp70brbwqg-elm-core-1.0.5"
                in
                nixPrefetchUrl (toUrl "elm/core" "1.0.5") "elm-core-1.0.5" `shouldReturn` Right output

        describe "invalid input" $
            it "fails when the archive doesn't exist" $ do
                Left (ProcessError err) <- nixPrefetchUrl (toUrl "elm/core" "0.0.0") "elm-core-0.0.0"
                err `shouldContain` "unable to download 'https://github.com/elm/core/archive/0.0.0.tar.gz'"


toUrl :: String -> String -> Url
toUrl name version =
    "https://github.com/" ++ name ++ "/archive/" ++ version ++ ".tar.gz"
