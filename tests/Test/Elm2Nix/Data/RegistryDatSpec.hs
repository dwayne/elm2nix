{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.Data.RegistryDatSpec (spec) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Elm2Nix.Data.Bytes.Encode as BE
import qualified Elm2Nix.Data.Name as Name
import qualified Elm2Nix.Data.RegistryDat as RegistryDat

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.Version (Version(..))
import Test.Hspec


spec :: Spec
spec =
  describe "Elm2Nix.Data.RegistryDat" $ do
    fromListSpec
    toAllPackagesSpec
    binarySerializationV0_19_1Spec
    binarySerializationV0_19_3Spec


fromListSpec :: Spec
fromListSpec =
  describe "fromList" $ do
    it "example 1" $
      let
        dependencies =
          [ Dependency Name.elmBrowser (Version 1 0 2)
          , Dependency Name.elmCore (Version 1 0 5)
          , Dependency Name.elmHtml (Version 1 0 0)
          ]

        registryDat = RegistryDat.fromList dependencies

        expectedCount = 3

        expectedPackages =
          Map.fromList
            [ ( Name.elmBrowser, [ Version 1 0 2 ] )
            , ( Name.elmCore, [ Version 1 0 5 ] )
            , ( Name.elmHtml, [ Version 1 0 0 ] )
            ]
      in do
      RegistryDat.toCount registryDat `shouldBe` expectedCount
      RegistryDat.toPackages registryDat `shouldBe` expectedPackages

    it "example 2" $
      let
        dependencies =
          [ Dependency Name.elmBrowser (Version 1 0 0)
          , Dependency Name.elmBrowser (Version 1 0 1)
          , Dependency Name.elmBrowser (Version 1 0 2)
          , Dependency Name.elmCore (Version 1 0 0)
          , Dependency Name.elmCore (Version 1 0 0)
          , Dependency Name.elmCore (Version 1 0 5)
          ]

        registryDat = RegistryDat.fromList dependencies

        expectedCount = 5

        expectedPackages =
          Map.fromList
            [ ( Name.elmBrowser, [ Version 1 0 2, Version 1 0 1, Version 1 0 0 ] )
            , ( Name.elmCore, [ Version 1 0 5, Version 1 0 0 ] )
            ]
      in do
      RegistryDat.toCount registryDat `shouldBe` expectedCount
      RegistryDat.toPackages registryDat `shouldBe` expectedPackages


toAllPackagesSpec :: Spec
toAllPackagesSpec =
  describe "toAllPackages" $
    it "example 1" $
      let
        dependencies =
          [ Dependency Name.elmBrowser (Version 1 0 0)
          , Dependency Name.elmBrowser (Version 1 0 1)
          , Dependency Name.elmBrowser (Version 1 0 2)
          , Dependency Name.elmCore (Version 1 0 0)
          , Dependency Name.elmCore (Version 1 0 0)
          , Dependency Name.elmCore (Version 1 0 5)
          ]

        registryDat = RegistryDat.fromList dependencies

        expectedAllPackages =
          [ ( "elm/browser", [ "1.0.0", "1.0.1", "1.0.2" ] )
          , ( "elm/core", [ "1.0.0", "1.0.5" ] )
          ]
      in
      RegistryDat.toAllPackages registryDat `shouldBe` expectedAllPackages


binarySerializationV0_19_1Spec :: Spec
binarySerializationV0_19_1Spec =
  describe "binary serialization for Elm 0.19.1" $ do
    describe "encode" $
      it "example 1" $
        let
          dependencies =
            [ Dependency Name.elmCore (Version 1 0 0)
            , Dependency Name.elmBrowser (Version 1 0 2)
            , Dependency Name.elmHtml (Version 1 0 0)
            , Dependency Name.elmCore (Version 1 0 5)
            ]

          registryDat = RegistryDat.fromList dependencies

          expectedByteString =
            LBS.pack
              -- count = 4 (8 bytes)
              [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04

              -- size = 3 (8 bytes)
              , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03

              -- author = elm, package = browser
              , 0x03, 0x65, 0x6C, 0x6D, 0x07, 0x62, 0x72, 0x6F, 0x77, 0x73, 0x65, 0x72
              -- version = 1 0 2
              , 0x01, 0x00, 0x02
              -- []
              , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00

              -- author = elm, package = core
              , 0x03, 0x65, 0x6C, 0x6D, 0x04, 0x63, 0x6F, 0x72, 0x65
              -- version = 1 0 5
              , 0x01, 0x00, 0x05
              -- [ version = 1 0 0 ]
              , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01
              , 0x01, 0x00, 0x00

              -- author = elm, package = html
              , 0x03, 0x65, 0x6C, 0x6D, 0x04, 0x68, 0x74, 0x6D, 0x6C
              -- version = 1 0 0
              , 0x01, 0x00, 0x00
              -- []
              , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
              ]
        in
        BE.encode (RegistryDat.binaryEncoderV0_19_1 registryDat) `shouldBe` expectedByteString


binarySerializationV0_19_3Spec :: Spec
binarySerializationV0_19_3Spec =
  describe "binary serialization for Elm 0.19.3" $ do
    describe "encode" $
      it "example 1" $
        let
          dependencies =
            [ Dependency Name.elmCore (Version 1 0 0)
            , Dependency Name.elmBrowser (Version 1 0 2)
            , Dependency Name.elmHtml (Version 1 0 0)
            , Dependency Name.elmCore (Version 1 0 5)
            ]

          registryDat = RegistryDat.fromList dependencies

          expectedByteString =
            LBS.pack
              -- count = 4 (8 bytes)
              [ 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00

              -- size = 3 (4 bytes)
              , 0x03, 0x00, 0x00, 0x00

              -- author = elm, package = browser
              , 0x03, 0x65, 0x6C, 0x6D, 0x07, 0x62, 0x72, 0x6F, 0x77, 0x73, 0x65, 0x72
              -- version = 1 0 2
              , 0x01, 0x00, 0x02
              -- []
              , 0x00, 0x00, 0x00, 0x00

              -- author = elm, package = core
              , 0x03, 0x65, 0x6C, 0x6D, 0x04, 0x63, 0x6F, 0x72, 0x65
              -- version = 1 0 5
              , 0x01, 0x00, 0x05
              -- [ version = 1 0 0 ]
              , 0x01, 0x00, 0x00, 0x00
              , 0x01, 0x00, 0x00

              -- author = elm, package = html
              , 0x03, 0x65, 0x6C, 0x6D, 0x04, 0x68, 0x74, 0x6D, 0x6C
              -- version = 1 0 0
              , 0x01, 0x00, 0x00
              -- []
              , 0x00, 0x00, 0x00, 0x00
              ]
        in
        BE.encode (RegistryDat.binaryEncoderV0_19_3 registryDat) `shouldBe` expectedByteString
