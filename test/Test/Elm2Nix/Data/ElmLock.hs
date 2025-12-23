{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.Data.ElmLock (main) where

import Test.Hspec

import qualified Elm2Nix.Data.ElmLock as ElmLock
import qualified Elm2Nix.Data.Name as Name
import qualified Elm2Nix.Lib.Json.Decode as JD
import qualified Test.Fixture as Fixture


import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.Version (Version(..))


main :: IO ()
main = hspec $
    describe "Elm2Nix.Data.ElmLock" $ do
        decoderSpec
        fromFileSpec


decoderSpec :: Spec
decoderSpec =
    describe "decoder" $ do
        describe "valid input" $
            it "example 1" $
                let
                    input =
                        "[                                 \
                        \    {                             \
                        \        \"author\": \"elm\",      \
                        \        \"package\": \"browser\", \
                        \        \"version\": \"1.0.2\"    \
                        \    },                            \
                        \    {                             \
                        \        \"author\": \"elm\",      \
                        \        \"package\": \"core\",    \
                        \        \"version\": \"1.0.5\"    \
                        \    },                            \
                        \    {                             \
                        \        \"author\": \"elm\",      \
                        \        \"package\": \"json\",    \
                        \        \"version\": \"1.1.3\"    \
                        \    },                            \
                        \    {                             \
                        \        \"author\": \"elm\",      \
                        \        \"package\": \"json\",    \
                        \        \"version\": \"1.1.4\"    \
                        \    }                             \
                        \]                                 "

                    elmLock =
                        ElmLock.fromList
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            , Dependency Name.elmCore (Version 1 0 5)
                            , Dependency Name.elmJson (Version 1 1 3)
                            , Dependency Name.elmJson (Version 1 1 4)
                            ]
                in
                JD.decodeString ElmLock.decoder input `shouldBe` Right elmLock


fromFileSpec :: Spec
fromFileSpec =
    describe "fromFile" $ do
        describe "valid input" $
            it "example 1" $
                let
                    elmLock =
                        ElmLock.fromList
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            , Dependency Name.elmCore (Version 1 0 5)
                            , Dependency Name.elmJson (Version 1 1 3)
                            , Dependency Name.elmJson (Version 1 1 4)
                            ]
                in
                (ElmLock.fromFile =<< Fixture.file "elm.lock") `shouldReturn` Right elmLock
