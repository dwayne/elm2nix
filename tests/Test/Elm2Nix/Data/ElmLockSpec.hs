{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.Data.ElmLockSpec (spec) where

import qualified Data.Json.Decode as JD
import qualified Elm2Nix.Data.ElmLock as ElmLock
import qualified Elm2Nix.Data.Name as Name

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.Version (Version(..))
import Test.Fixtures (fixture)
import Test.Hspec


spec :: Spec
spec =
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
        JD.decodeText ElmLock.decoder input `shouldBe` Right elmLock


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
        (ElmLock.fromFile =<< fixture "elm.lock") `shouldReturn` Right elmLock
