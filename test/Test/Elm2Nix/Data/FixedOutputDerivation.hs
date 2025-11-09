module Test.Elm2Nix.Data.FixedOutputDerivation (main) where

import Data.Bifunctor (second)
import Test.Hspec

import qualified Elm2Nix.Data.FixedOutputDerivation as FOD
import qualified Elm2Nix.Data.Name as Name

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.FixedOutputDerivation (FromDependenciesError)
import Elm2Nix.Data.Version (Version(..))
import Elm2Nix.Lib.Nix (NixPrefetchUrlError(..), Sha256)


main :: IO ()
main = hspec $
    describe "Elm2Nix.Data.FixedOutputDerivation" fromDependenciesSpec


fromDependenciesSpec :: Spec
fromDependenciesSpec =
    describe "fromDependencies (network)" $ do
        describe "valid input" $
            it "example 1" $
                let
                    dependencies =
                        [ Dependency Name.elmBrowser (Version 1 0 2)
                        , Dependency Name.elmCore (Version 1 0 5)
                        ]

                    result =
                        [ ( "0863nw2hhbpm3s03lm1imi5x28wwknzrwg2p79s5mydgvdvgwjf0", "/nix/store/q9j39gnm3saa7xczrcs2wfym7mi7468d-elm-browser-1.0.2" )
                        , ( "0g3xbi8f9k5q45s95nx3jfvzwdf4b2n63a52wr4027d2xjx0pmvl", "/nix/store/y1h9ay7hd8sij79sd41344gp70brbwqg-elm-core-1.0.5" )
                        ]
                in
                fromDependencies dependencies `shouldReturn` Right result

        describe "invalid input" $
            it "example 1" $
                let
                    elmCore = Dependency Name.elmCore (Version 0 0 0)

                    dependencies =
                        [ Dependency Name.elmBrowser (Version 1 0 2)
                        , elmCore
                        ]
                in do
                Left [( dependency, ProcessError err )] <- fromDependencies dependencies

                dependency `shouldBe` elmCore
                err `shouldContain` "unable to download 'https://github.com/elm/core/archive/0.0.0.tar.gz'"


fromDependencies :: [Dependency] -> IO (Either FromDependenciesError [(Sha256, FilePath)])
fromDependencies =
    fmap (second $ map $ (,) <$> FOD.toHash <*> FOD.toPath) . FOD.fromDependencies
