{-# LANGUAGE OverloadedStrings #-}

module Test.Elm2Nix.Data.ElmJson (main) where

import Data.List (isSuffixOf)
import System.IO.Error (isDoesNotExistError)
import Test.Hspec

import qualified Elm2Nix.Data.ElmJson as ElmJson
import qualified Elm2Nix.Data.Name as Name
import qualified Elm2Nix.Lib.Json.Decode as JD
import qualified Test.Fixture as Fixture


import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.Version (Version(..))


main :: IO ()
main = hspec $
    describe "Elm2Nix.Data.ElmJson" $ do
        dependenciesDecoderSpec
        decoderSpec
        fromFileSpec
        fromFilesSpec


dependenciesDecoderSpec :: Spec
dependenciesDecoderSpec =
    describe "dependenciesDecoder" $ do
        it "example 1" $
            JD.decodeString ElmJson.dependenciesDecoder "{}" `shouldBe` Right []

        it "example 2" $
            let
                input =
                    "{                               \
                    \    \"elm/browser\": \"1.0.2\", \
                    \    \"elm/core\": \"1.0.5\",    \
                    \    \"elm/html\": \"1.0.0\",    \
                    \    \"elm/json\": \"1.1.3\",    \
                    \    \"elm/url\": \"1.0.0\"      \
                    \}                               "

                output =
                    [ Dependency Name.elmBrowser (Version 1 0 2)
                    , Dependency Name.elmCore (Version 1 0 5)
                    , Dependency Name.elmHtml (Version 1 0 0)
                    , Dependency Name.elmJson (Version 1 1 3)
                    , Dependency Name.elmUrl (Version 1 0 0)
                    ]
            in
            JD.decodeString ElmJson.dependenciesDecoder input `shouldBe` Right output

        it "example 3" $
            let
                input =
                    "{                           \
                    \    \"/browser\": \"1.0.2\" \
                    \}                           "
            in
            JD.decodeString ElmJson.dependenciesDecoder input `shouldBe` Left (JD.DecodeError (JD.FieldError "/browser" (JD.Failure "author is empty")))

        it "example 4" $
            let
                input =
                    "{                            \
                    \    \"elm/browser\": \"1.0\" \
                    \}                            "
            in
            JD.decodeString ElmJson.dependenciesDecoder input `shouldBe` Left (JD.DecodeError (JD.FieldError "elm/browser" (JD.Failure "version is invalid: 1.0")))


decoderSpec :: Spec
decoderSpec =
    describe "decoder" $ do
        describe "valid input" $ do
            it "example 1" $
                let
                    input =
                        "{                                      \
                        \    \"type\": \"application\",         \
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
                        \            \"elm/html\": \"1.0.0\"    \
                        \         },                            \
                        \        \"indirect\": {                \
                        \            \"elm/json\": \"1.1.3\"    \
                        \         }                             \
                        \    }                                  \
                        \}                                      "

                    elmJson =
                        ElmJson.fromList
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            , Dependency Name.elmCore (Version 1 0 5)
                            , Dependency Name.elmHtml (Version 1 0 0)
                            , Dependency Name.elmJson (Version 1 1 3)
                            ]
                in
                JD.decodeString ElmJson.decoder input `shouldBe` Right elmJson

            it "example 2" $
                let
                    input =
                        "{                                      \
                        \    \"type\": \"application\",         \
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
                        \            \"elm/html\": \"1.0.0\"    \
                        \         }                             \
                        \    }                                  \
                        \}                                      "

                    elmJson =
                        ElmJson.fromList
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            , Dependency Name.elmCore (Version 1 0 5)
                            , Dependency Name.elmHtml (Version 1 0 0)
                            ]
                in
                JD.decodeString ElmJson.decoder input `shouldBe` Right elmJson

            it "example 3" $
                let
                    input =
                        "{                                      \
                        \    \"type\": \"application\",         \
                        \    \"dependencies\": {                \
                        \        \"direct\": {                  \
                        \            \"elm/browser\": \"1.0.2\" \
                        \        },                             \
                        \        \"indirect\": {                \
                        \            \"elm/core\": \"1.0.5\"    \
                        \        }                              \
                        \    },                                 \
                        \    \"test-dependencies\": {           \
                        \    }                                  \
                        \}                                      "

                    elmJson =
                        ElmJson.fromList
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            , Dependency Name.elmCore (Version 1 0 5)
                            ]
                in
                JD.decodeString ElmJson.decoder input `shouldBe` Right elmJson

            it "example 4" $
                let
                    input =
                        "{                                      \
                        \    \"type\": \"application\",         \
                        \    \"dependencies\": {                \
                        \        \"direct\": {                  \
                        \            \"elm/browser\": \"1.0.2\" \
                        \        },                             \
                        \        \"indirect\": {                \
                        \            \"elm/core\": \"1.0.5\"    \
                        \        }                              \
                        \    }                                  \
                        \}                                      "

                    elmJson =
                        ElmJson.fromList
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            , Dependency Name.elmCore (Version 1 0 5)
                            ]
                in
                JD.decodeString ElmJson.decoder input `shouldBe` Right elmJson

            it "example 5" $
                let
                    input =
                        "{                                      \
                        \    \"type\": \"application\",         \
                        \    \"dependencies\": {                \
                        \    \"type\": \"application\",         \
                        \        \"direct\": {                  \
                        \            \"elm/browser\": \"1.0.2\" \
                        \        }                              \
                        \    }                                  \
                        \}                                      "

                    elmJson =
                        ElmJson.fromList
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            ]
                in
                JD.decodeString ElmJson.decoder input `shouldBe` Right elmJson

            it "example 6" $
                let
                    input =
                        "{                              \
                        \    \"type\": \"application\", \
                        \    \"dependencies\": {        \
                        \        \"direct\": {          \
                        \        }                      \
                        \    }                          \
                        \}                              "

                    elmJson =
                        ElmJson.fromList []
                in
                JD.decodeString ElmJson.decoder input `shouldBe` Right elmJson

            it "example 7" $
                let
                    input =
                        "{                              \
                        \    \"type\": \"application\", \
                        \    \"dependencies\": {        \
                        \    }                          \
                        \}                              "

                    elmJson = ElmJson.fromList []
                in
                JD.decodeString ElmJson.decoder input `shouldBe` Right elmJson

            it "example 8" $
                let
                    input = "{ \"type\": \"application\" }"
                    elmJson = ElmJson.fromList []
                in
                JD.decodeString ElmJson.decoder input `shouldBe` Right elmJson

            it "example 9" $
                let
                    input =
                        "{                                      \
                        \    \"type\": \"application\",         \
                        \    \"dependencies\": {                \
                        \        \"direct\": {                  \
                        \            \"elm/html\": \"1.0.0\"    \
                        \        },                             \
                        \        \"indirect\": {                \
                        \            \"elm/json\": \"1.1.3\"    \
                        \        }                              \
                        \    },                                 \
                        \    \"test-dependencies\": {           \
                        \        \"direct\": {                  \
                        \            \"elm/browser\": \"1.0.2\" \
                        \         },                            \
                        \        \"indirect\": {                \
                        \            \"elm/core\": \"1.0.5\"    \
                        \         }                             \
                        \    }                                  \
                        \}                                      "

                    elmJson =
                        ElmJson.fromList
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            , Dependency Name.elmCore (Version 1 0 5)
                            , Dependency Name.elmHtml (Version 1 0 0)
                            , Dependency Name.elmJson (Version 1 1 3)
                            ]
                in
                JD.decodeString ElmJson.decoder input `shouldBe` Right elmJson

            it "example 10" $
                let
                    input =
                        "{                                      \
                        \    \"type\": \"application\",         \
                        \    \"dependencies\": {                \
                        \        \"direct\": {                  \
                        \            \"elm/browser\": \"1.0.2\" \
                        \        },                             \
                        \        \"indirect\": {                \
                        \            \"elm/browser\": \"1.0.2\" \
                        \        }                              \
                        \    }                                  \
                        \}                                      "

                    elmJson =
                        ElmJson.fromList
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            ]
                in
                JD.decodeString ElmJson.decoder input `shouldBe` Right elmJson


fromFileSpec :: Spec
fromFileSpec =
    describe "fromFile" $ do
        describe "valid input" $
            it "example 1" $
                let
                    elmJson =
                        ElmJson.fromList
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            , Dependency Name.elmCore (Version 1 0 5)
                            , Dependency Name.elmHtml (Version 1 0 0)
                            , Dependency Name.elmJson (Version 1 1 3)
                            , Dependency Name.elmTime (Version 1 0 0)
                            , Dependency Name.elmUrl (Version 1 0 0)
                            , Dependency Name.elmVirtualDom (Version 1 0 3)
                            ]
                in
                (ElmJson.fromFile =<< Fixture.file "elm.json") `shouldReturn` Right elmJson

        describe "invalid input" $ do
            it "when the file does not exist" $
                ElmJson.fromFile "path/to/missing/elm.json" `shouldThrow` isDoesNotExistError

            it "when / is missing" $ do
                path <- Fixture.file "name-missing-forward-slash.json"
                Left err <- ElmJson.fromFile path
                err `shouldBe` JD.DecodeError (JD.FieldError "dependencies.direct" (JD.FieldError "elmbrowser" (JD.Failure "/ is missing")))

            it "when version is incorrectly formatted" $ do
                path <- Fixture.file "version-incorrect-format.json"
                Left err <- ElmJson.fromFile path
                err `shouldBe` JD.DecodeError (JD.FieldError "dependencies.direct" (JD.FieldError "elm/browser" (JD.Failure "version is invalid: 1.0")))

            it "when version has a part with leading zeros" $ do
                path <- Fixture.file "version-leading-zeros.json"
                Left err <- ElmJson.fromFile path
                err `shouldBe` JD.DecodeError (JD.FieldError "dependencies.direct" (JD.FieldError "elm/browser" (JD.Failure "version is invalid: 1.0.02")))


fromFilesSpec :: Spec
fromFilesSpec =
    describe "fromFiles" $ do
        describe "valid input" $
            it "example 1" $
                let
                    elmJson =
                        ElmJson.fromList
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            , Dependency Name.elmCore (Version 1 0 5)
                            , Dependency Name.elmHtml (Version 1 0 0)
                            ]
                in
                (ElmJson.fromFiles =<< traverse Fixture.file [ "elm1.json", "elm2.json", "elm3.json" ]) `shouldReturn` Right elmJson

        describe "invalid input" $ do
            it "when one of the files does not exist" $ do
                paths <- traverse Fixture.file [ "elm1.json", "path/to/missing/elm.json", "elm3.json" ]
                ElmJson.fromFiles paths `shouldThrow` isDoesNotExistError

            it "when there is an error in one of the files" $ do
                paths <- traverse Fixture.file [ "elm1.json", "name-missing-forward-slash.json", "elm3.json" ]
                Left ( badPath, err ) <- ElmJson.fromFiles paths
                ("name-missing-forward-slash.json" `isSuffixOf` badPath) `shouldBe` True
                err `shouldBe` JD.DecodeError (JD.FieldError "dependencies.direct" (JD.FieldError "elmbrowser" (JD.Failure "/ is missing")))
