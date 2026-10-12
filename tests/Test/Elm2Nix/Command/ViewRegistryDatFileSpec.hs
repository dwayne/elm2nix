{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.Command.ViewRegistryDatFileSpec (spec) where

import qualified Elm2Nix.Command.ViewRegistryDatFile as ViewRegistryDatFile
import qualified Elm2Nix.Data.Name as Name
import qualified Elm2Nix.Data.RegistryDat as RegistryDat
import qualified Elm2Nix.Data.RegistryDat.Decode as RDD

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.RegistryDat.ElmVersion (ElmVersion(..))
import Elm2Nix.Data.Version (Version(..))
import Test.Fixtures (fixture)
import Test.Hspec


spec :: Spec
spec =
  describe "Elm2Nix.Command.ViewRegistryDatFile" toRegistryDatSpec


toRegistryDatSpec :: Spec
toRegistryDatSpec =
  describe "render (skip)" $ do
    describe "when Elm 0.19.1" $
      let
        view = ViewRegistryDatFile.toRegistryDat V0_19_1
      in do
      describe "valid input" $
        it "example 1" $
          let
            registryDat =
              RegistryDat.fromList
                [ Dependency Name.elmBrowser (Version 1 0 2)
                , Dependency Name.elmCore (Version 1 0 5)
                , Dependency Name.elmHtml (Version 1 0 0)
                , Dependency Name.elmJson (Version 1 1 3)
                , Dependency Name.elmTime (Version 1 0 0)
                , Dependency Name.elmUrl (Version 1 0 0)
                , Dependency Name.elmVirtualDom (Version 1 0 3)
                ]
          in
          (view =<< fixture "0.19.1/registry.dat") `shouldReturn` Right registryDat

      describe "invalid input" $ do
        it "when the file does not exist" $
          view "path/to/missing/registry.dat"
          `shouldReturn`
          Left (ViewRegistryDatFile.FileNotFound "path/to/missing/registry.dat")

        it "when the file is corrupted" $ do
          file <- fixture "0.19.1/corrupted.dat"
          Left (ViewRegistryDatFile.DecodeError _ (RDD.Failure _ _ message)) <- view file
          message `shouldBe` "not enough bytes"

    describe "when Elm 0.19.3" $
      let
        view = ViewRegistryDatFile.toRegistryDat V0_19_3
      in do
      describe "valid input" $
        it "example 1" $
          let
            registryDat =
              RegistryDat.fromList
                [ Dependency Name.elmBrowser (Version 1 0 2)
                , Dependency Name.elmCore (Version 1 0 5)
                , Dependency Name.elmHtml (Version 1 0 0)
                , Dependency Name.elmJson (Version 1 1 3)
                , Dependency Name.elmTime (Version 1 0 0)
                , Dependency Name.elmUrl (Version 1 0 0)
                , Dependency Name.elmVirtualDom (Version 1 0 3)
                ]
          in
          (view =<< fixture "0.19.3/registry.dat") `shouldReturn` Right registryDat

      describe "invalid input" $ do
        it "when the file does not exist" $
          view "path/to/missing/registry.dat"
          `shouldReturn`
          Left (ViewRegistryDatFile.FileNotFound "path/to/missing/registry.dat")

        it "when the file is corrupted" $ do
          file <- fixture "0.19.3/corrupted.dat"
          Left (ViewRegistryDatFile.DecodeError _ (RDD.Failure _ _ message)) <- view file
          message `shouldBe` "not enough bytes"
