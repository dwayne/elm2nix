module Test.Elm2Nix.Lib.Json.Decode (main) where

import qualified Elm2Nix.Lib.Json.Decode as JD

import Data.Maybe (fromMaybe)
import Test.Hspec


main :: IO ()
main = hspec $
    describe "Elm2Nix.Lib.Json.Decode" $ do
        it "string" $
            JD.decodeString JD.string "\"apple\"" `shouldBe` Right "apple"

        it "literal" $ do
            JD.decodeString (JD.literal "apple") "\"apple\"" `shouldBe` Right ()
            JD.decodeString (JD.literal "apple") "\"apples\"" `shouldBe` Left (JD.DecodeError (JD.Failure "not equal to \"apple\": \"apples\""))

        it "keyValuePairs" $
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

        it "field" $
            let
                input = "{ \"person\": { \"name\": \"tom\", \"age\": 42 } }"
            in
            JD.decodeString (JD.field "person" (JD.field "name" JD.string)) input `shouldBe` Right "tom"

        it "at" $
            let
                input = "{ \"person\": { \"name\": \"tom\", \"age\": 42 } }"
            in
            JD.decodeString (JD.at [ "person", "name" ] JD.string) input `shouldBe` Right "tom"

        it "elmJsonDecoder" $
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
        <$> pathToDependenciesDecoder ["dependencies", "direct"]
        <*> pathToDependenciesDecoder ["dependencies", "indirect"]
        <*> pathToDependenciesDecoder ["test-dependencies", "direct"]
        <*> pathToDependenciesDecoder ["test-dependencies", "indirect"]


pathToDependenciesDecoder :: [String] -> JD.Decoder [(String, String)]
pathToDependenciesDecoder dottedPath =
    fmap (fromMaybe []) (JD.optional (JD.at dottedPath (JD.keyValuePairs Right JD.string)))
