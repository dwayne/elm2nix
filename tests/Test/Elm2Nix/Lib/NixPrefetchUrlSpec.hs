module Test.Elm2Nix.Lib.NixPrefetchUrlSpec (spec) where

import qualified Elm2Nix.Lib.NixPrefetchUrl as NixPrefetchUrl

import Test.Hspec


spec :: Spec
spec =
  describe "Elm2Nix.Lib.NixPrefetchUrl" runSpec


runSpec :: Spec
runSpec =
  describe "run (skip) (skip:network)" $ do
    describe "valid input" $
      it "example 1" $
        let
          output = NixPrefetchUrl.Output hash path
          hash = "0g3xbi8f9k5q45s95nx3jfvzwdf4b2n63a52wr4027d2xjx0pmvl"
          path = "/nix/store/y1h9ay7hd8sij79sd41344gp70brbwqg-elm-core-1.0.5"
        in
        NixPrefetchUrl.run (toUrl "elm/core" "1.0.5") "elm-core-1.0.5" `shouldReturn` Right output

    describe "invalid input" $
      it "fails when the archive doesn't exist" $ do
        Left (NixPrefetchUrl.ProcessError err) <- NixPrefetchUrl.run (toUrl "elm/core" "0.0.0") "elm-core-0.0.0"
        err `shouldContain` "unable to download 'https://github.com/elm/core/archive/0.0.0.tar.gz'"


toUrl :: String -> String -> NixPrefetchUrl.Url
toUrl name version =
  "https://github.com/" ++ name ++ "/archive/" ++ version ++ ".tar.gz"
