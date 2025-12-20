module Test.Elm2Nix.Lib.Json.Decode (main) where

import qualified Elm2Nix.Lib.Json.Decode as JD

import Data.Maybe (fromMaybe)
import Test.Hspec


main :: IO ()
main = hspec $
    describe "Elm2Nix.Lib.Json.Decode" $ do
        it "example 1" $
            JD.decodeString JD.string "\"apple\"" `shouldBe` Right "apple"

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
                    [ ( "elm/browser", "1.0.2" )
                    , ( "elm/core", "1.0.5" )
                    , ( "elm/html", "1.0.0" )
                    , ( "elm/json", "1.1.3" )
                    , ( "elm/url", "1.0.0" )
                    ]
            in
            JD.decodeString (JD.keyValuePairs Right JD.string) input `shouldBe` Right output

        it "example 3" $
            let
                input = "{ \"person\": { \"name\": \"tom\", \"age\": 42 } }"
            in
            JD.decodeString (JD.field "person" (JD.field "name" JD.string)) input `shouldBe` Right "tom"

        it "example 4" $
            let
                input = "{ \"person\": { \"name\": \"tom\", \"age\": 42 } }"
            in
            JD.decodeString (JD.at [ "person", "name" ] JD.string) input `shouldBe` Right "tom"

        it "example 5" $
            let
                input =
                    "{                                       \
                    \    \"dependencies\": {                 \
                    \        \"direct\": {                   \
                    \            \"elm/browser\": \"1.0.2\", \
                    \            \"elm/core\": \"1.0.5\",    \
                    \            \"elm/html\": \"1.0.0\"     \
                    \        },                              \
                    \        \"indirect\": {}                \
                    \    },                                  \
                    \    \"test-dependencies\": {            \
                    \        \"direct\": {                   \
                    \            \"elm/json\": \"1.1.3\",    \
                    \            \"elm/url\": \"1.0.0\"      \
                    \        }                               \
                    \    }                                   \
                    \}                                       "

                output =
                    [ ( "elm/browser", "1.0.2" )
                    , ( "elm/core", "1.0.5" )
                    , ( "elm/html", "1.0.0" )
                    , ( "elm/json", "1.1.3" )
                    , ( "elm/url", "1.0.0" )
                    ]
            in
            JD.decodeString elmJsonDecoder input `shouldBe` Right output


elmJsonDecoder :: JD.Decoder [(String, String)]
elmJsonDecoder =
    (\a b c d -> a ++ b ++ c ++ d)
        <$> dependencyDecoder ["dependencies", "direct"]
        <*> dependencyDecoder ["dependencies", "indirect"]
        <*> dependencyDecoder ["test-dependencies", "direct"]
        <*> dependencyDecoder ["test-dependencies", "indirect"]


dependencyDecoder :: [String] -> JD.Decoder [(String, String)]
dependencyDecoder dottedPath =
    fmap (fromMaybe []) (JD.optional (JD.at dottedPath (JD.keyValuePairs Right JD.string)))
