{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.ElmJson (main) where

import qualified Data.Aeson as Json
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Set as Set
import qualified Data.Text as Text

import Data.Aeson (Key, Value)
import Data.Bifunctor (second)
import Data.Text (Text)
import Test.Hspec

import Elm2Nix.Dependency (Dependency(..))
import Elm2Nix.ElmJson (getDependencies)
import Elm2Nix.Version (Version(..))


main :: IO ()
main = hspec $
    describe "Elm2Nix.ElmJson" getDependenciesSpec


getDependenciesSpec :: Spec
getDependenciesSpec =
    describe "getDependencies" $ do
        it "combines each of dependencies.{direct, indirect} and test-dependencies.{direct, indirect}" $
            let
                elmDotJson =
                    mkElmDotJson $ Options
                        { dependencies = Just $ Constraints
                            { direct = Just
                                [ ( "elm/browser", "1.0.2" )
                                , ( "elm/core", "1.0.5" )
                                , ( "elm/html", "1.0.0" )
                                , ( "elm/json", "1.1.3" )
                                , ( "elm/url", "1.0.0" )
                                ]
                            , indirect = Just
                                [ ( "elm/time", "1.0.0" )
                                , ( "elm/virtual-dom", "1.0.3" )
                                ]
                            }
                        , testDependencies = Just $ Constraints
                            { direct = Just
                                [ ( "elm-explorations/test", "2.2.0" )
                                ]
                            , indirect = Just
                                [ ( "elm/random", "1.0.0" )
                                ]
                            }
                        }

                dependencies =
                    Set.fromList
                        [ Dependency "elm" "browser" (Version 1 0 2)
                        , Dependency "elm" "core" (Version 1 0 5)
                        , Dependency "elm" "html" (Version 1 0 0)
                        , Dependency "elm" "json" (Version 1 1 3)
                        , Dependency "elm" "url" (Version 1 0 0)
                        , Dependency "elm" "time" (Version 1 0 0)
                        , Dependency "elm" "virtual-dom" (Version 1 0 3)
                        , Dependency "elm-explorations" "test" (Version 2 2 0)
                        , Dependency "elm" "random" (Version 1 0 0)
                        ]
            in
            getDependencies elmDotJson `shouldBe` Right dependencies

        it "allows fields to be empty" $
            let
                elmDotJson =
                    mkElmDotJson $ Options
                        { dependencies = Just $ Constraints
                            { direct = Just
                                [ ( "elm/browser", "1.0.2" )
                                ]
                            , indirect = Just []
                            }
                        , testDependencies = Just $ Constraints
                            { direct = Just []
                            , indirect = Just []
                            }
                        }

                dependencies =
                    Set.fromList
                        [ Dependency "elm" "browser" (Version 1 0 2)
                        ]
            in
            getDependencies elmDotJson `shouldBe` Right dependencies


        it "allows fields to be missing" $
            let
                elmDotJson =
                    mkElmDotJson $ Options
                        { dependencies = Just $ Constraints
                            { direct = Just
                                [ ( "elm/browser", "1.0.2" )
                                ]
                            , indirect = Nothing
                            }
                        , testDependencies = Nothing
                        }

                dependencies =
                    Set.fromList
                        [ Dependency "elm" "browser" (Version 1 0 2)
                        ]
            in
            getDependencies elmDotJson `shouldBe` Right dependencies

        it "fails when at least one author/package name is invalid" $
            let
                elmDotJson =
                    mkElmDotJson $ Options
                        { dependencies = Just $ Constraints
                            { direct = Just
                                [ ( "elmbrowser", "1.0.2" )
                                ]
                            , indirect = Nothing
                            }
                        , testDependencies = Nothing
                        }
            in
            getDependencies elmDotJson `shouldBe` Left "Error in $.dependencies.direct.elmbrowser: / is missing"

        it "fails when at least one version is invalid" $
            let
                elmDotJson =
                    mkElmDotJson $ Options
                        { dependencies = Just $ Constraints
                            { direct = Just
                                [ ( "elm/browser", "1.0" )
                                ]
                            , indirect = Nothing
                            }
                        , testDependencies = Nothing
                        }
            in
            getDependencies elmDotJson `shouldBe` Left "Error in $.dependencies.direct['elm/browser']: version is incorrectly formatted: 1.0"


-- HELPERS


data Options
    = Options
        { dependencies :: Maybe Constraints
        , testDependencies :: Maybe Constraints
        }


type Entry = ( Key, Text )


data Constraints
    = Constraints
        { direct :: Maybe [Entry]
        , indirect :: Maybe [Entry]
        }


mkElmDotJson :: Options -> Value
mkElmDotJson (Options dependencies testDependencies) =
    Json.Object $ KeyMap.fromList $
        [ ( "type", Json.String "application" )
        ]
        ++
        addConstraints "dependencies" dependencies
        ++
        addConstraints "test-dependencies" testDependencies

    where
        addConstraints :: Key -> Maybe Constraints -> [( Key, Value )]
        addConstraints key constraints =
            case constraints of
                Just (Constraints direct indirect) ->
                    [ ( key
                      , Json.Object $ KeyMap.fromList $
                            addEntries "direct" direct
                            ++
                            addEntries "indirect" indirect
                      )
                    ]

                Nothing ->
                    []

        addEntries :: Key -> Maybe [Entry] -> [( Key, Value )]
        addEntries key entries =
            case entries of
                Just list ->
                    [ ( key
                      , Json.Object $ KeyMap.fromList $ map (second Json.String) list
                      )
                    ]

                Nothing ->
                    []
