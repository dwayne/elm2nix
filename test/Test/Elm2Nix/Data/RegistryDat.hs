module Test.Elm2Nix.Data.RegistryDat (main) where

import Test.Hspec

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Elm2Nix.Data.Name as Name
import qualified Elm2Nix.Data.RegistryDat as RegistryDat

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.Version (Version(..))


main :: IO ()
main = hspec $
    describe "Elm2Nix.Data.RegistryDat" fromListSpec


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
                        [ ( Name.elmBrowser, Set.singleton (Version 1 0 2) )
                        , ( Name.elmCore, Set.singleton (Version 1 0 5) )
                        , ( Name.elmHtml, Set.singleton (Version 1 0 0) )
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
                        [ ( Name.elmBrowser, Set.fromList [ Version 1 0 0, Version 1 0 1, Version 1 0 2 ] )
                        , ( Name.elmCore, Set.fromList [ Version 1 0 0, Version 1 0 5 ] )
                        ]
            in do
            RegistryDat.toCount registryDat `shouldBe` expectedCount
            RegistryDat.toPackages registryDat `shouldBe` expectedPackages
