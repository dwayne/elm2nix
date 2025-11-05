{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.Data.ElmJson (main) where

import qualified Data.Aeson as Json

import Data.ByteString.Lazy (ByteString)
import Test.Hspec

import qualified Elm2Nix.Data.ElmJson as ElmJson
import qualified Elm2Nix.Data.Name as Name

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.ElmJson (ElmJson)
import Elm2Nix.Data.Version (Version(..))


main :: IO ()
main = hspec $
    describe "Elm2Nix.Data.ElmJson" eitherDecodeSpec


eitherDecodeSpec :: Spec
eitherDecodeSpec =
    describe "eitherDecode" $
        describe "valid input" $
            it "example 1" $
                let
                    input =
                        "{                                      \
                        \    \"dependencies\": {                \
                        \        \"direct\": {                  \
                        \            \"elm/browser\": \"1.0.2\" \
                        \        },                             \
                        \        \"indirect\": {                \
                        \            \"elm/core\": \"1.0.5\"    \
                        \        }                              \
                        \    },                                 \
                        \    \"test-dependencies\": {           \
                        \        \"direct\": {                  \
                        \             \"elm/html\": \"1.0.0\"   \
                        \         },                            \
                        \        \"indirect\": {                \
                        \             \"elm/json\": \"1.1.3\"   \
                        \         }                             \
                        \    }                                  \
                        \}                                      "

                    elmJson =
                        ElmJson.fromDependencies
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            , Dependency Name.elmCore (Version 1 0 5)
                            , Dependency Name.elmHtml (Version 1 0 0)
                            , Dependency Name.elmJson (Version 1 1 3)
                            ]
                in
                eitherDecode input `shouldBe` Right elmJson


eitherDecode :: ByteString -> Either String ElmJson
eitherDecode = Json.eitherDecode
