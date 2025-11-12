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
    describe "eitherDecode" $ do
        describe "valid input" $ do
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
                eitherDecode input `shouldBe` Right elmJson

            it "example 2" $
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
                eitherDecode input `shouldBe` Right elmJson

            it "example 3" $
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
                        \    }                                  \
                        \}                                      "

                    elmJson =
                        ElmJson.fromList
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            , Dependency Name.elmCore (Version 1 0 5)
                            ]
                in
                eitherDecode input `shouldBe` Right elmJson

            it "example 4" $
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
                        \    }                                  \
                        \}                                      "

                    elmJson =
                        ElmJson.fromList
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            , Dependency Name.elmCore (Version 1 0 5)
                            ]
                in
                eitherDecode input `shouldBe` Right elmJson

            it "example 5" $
                let
                    input =
                        "{                                      \
                        \    \"dependencies\": {                \
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
                eitherDecode input `shouldBe` Right elmJson

            it "example 6" $
                let
                    input =
                        "{                       \
                        \    \"dependencies\": { \
                        \        \"direct\": {   \
                        \        }               \
                        \    }                   \
                        \}                       "

                    elmJson =
                        ElmJson.fromList []
                in
                eitherDecode input `shouldBe` Right elmJson

            it "example 7" $
                let
                    input =
                        "{                       \
                        \    \"dependencies\": { \
                        \    }                   \
                        \}                       "

                    elmJson = ElmJson.fromList []
                in
                eitherDecode input `shouldBe` Right elmJson

            it "example 8" $
                let
                    input = "{}"
                    elmJson = ElmJson.fromList []
                in
                eitherDecode input `shouldBe` Right elmJson

            it "example 9" $
                let
                    input =
                        "{                                      \
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
                eitherDecode input `shouldBe` Right elmJson

            it "example 10" $
                let
                    input =
                        "{                                      \
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
                eitherDecode input `shouldBe` Right elmJson

        describe "invalid input" $ do
            it "example 1" $
                let
                    input =
                        "{                                     \
                        \    \"dependencies\": {               \
                        \        \"direct\": {                 \
                        \            \"elmbrowser\": \"1.0.2\" \
                        \        }                             \
                        \    }                                 \
                        \}                                     "
                in
                eitherDecode input `shouldBe` Left "Error in $.dependencies.direct.elmbrowser: / is missing"

            it "example 2" $
                let
                    input =
                        "{                                     \
                        \    \"dependencies\": {               \
                        \        \"direct\": {                 \
                        \            \"elm/browser\": \"10.2\" \
                        \        }                             \
                        \    }                                 \
                        \}                                     "
                in
                eitherDecode input `shouldBe` Left "Error in $.dependencies.direct['elm/browser']: version is invalid: \"10.2\""

            it "example 3" $
                let
                    input =
                        "{                                 \
                        \    \"dependencies\": {           \
                        \        \"direct\": {             \
                        \            \"elm/browser\": null \
                        \        }                         \
                        \    }                             \
                        \}                                 "
                in
                eitherDecode input `shouldBe` Left "Error in $.dependencies.direct['elm/browser']: parsing Version failed, expected String, but encountered Null"

            it "example 4" $
                let
                    input =
                        "{                        \
                        \    \"dependencies\": {  \
                        \        \"direct\": \"\" \
                        \    }                    \
                        \}                        "
                in
                eitherDecode input `shouldBe` Left "Error in $.dependencies.direct: parsing Dependencies failed, expected Object, but encountered String"

            it "example 5" $
                let
                    input =
                        "{                          \
                        \    \"dependencies\": \"\" \
                        \}                          "
                in
                eitherDecode input `shouldBe` Left "Error in $.dependencies: parsing DirectAndIndirect failed, expected Object, but encountered String"


eitherDecode :: ByteString -> Either String ElmJson
eitherDecode = Json.eitherDecode
