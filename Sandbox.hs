{-# LANGUAGE OverloadedStrings #-}

module Sandbox where


import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Char as Char
import qualified Data.Text as Text

import Data.Aeson.Types ((.:), (.:?), (.!=), Key, Parser, parseEither, parseFail)
import Data.Foldable.WithIndex (ifoldl)
import Data.Function ((&))
import Data.Text (Text)



elmJsonParser2 :: Json.Value -> Parser [Dependency]
elmJsonParser2 =
    Json.withObject "elm.json" $ \o ->
        let
            dependencies =
                ((o .: "dependencies") >>= Json.withObject "dependencies" (\o ->
                    (++) <$> ((o .: "direct") >>= dependenciesParser "dependencies.direct")
                         <*> ((o .: "indirect") >>= dependenciesParser "dependencies.indirect"))
                )

            testDependencies =
                ((o .:? "test-dependencies" .!= emptyObject) >>= Json.withObject "test-dependencies" (\o ->
                    (++) <$> ((o .:? "direct" .!= emptyObject) >>= dependenciesParser "test-dependencies.direct")
                         <*> ((o .:? "indirect" .!= emptyObject) >>= dependenciesParser "test-dependencies.indirect"))
                )
        in
        (++) <$> dependencies <*> testDependencies


emptyObject :: Json.Value
emptyObject =
    Json.Object KM.empty

--
-- Q: How to get the dependencies from direct and indirect and combine them all into one?
--
f :: Key -> Key -> (Json.Value -> Parser a) -> Json.Value -> Parser a
f key1 key2 parser =
    let
        name1 =
            Key.toString key1

        name2 =
            Key.toString key2
    in
    Json.withObject name1 $ \o1 ->
        (o1 .: key1) >>=
            (Json.withObject name2 $ \o2 ->
                (o2 .: key2) >>= parser
            )


elmJsonParser :: Json.Value -> Parser [Dependency]
elmJsonParser value =
    let
        direct =
            keyParser "dependencies" value >>= keyParser "direct" >>= dependenciesParser "abc"

        indirect =
            keyParser "dependencies" value >>= keyParser "indirect" >>= dependenciesParser "xyz"
    in
    (++) <$> direct <*> indirect


keyParser :: Key -> Json.Value -> Parser Json.Value
keyParser key =
    Json.withObject (Key.toString key) $ \o ->
        o .: key


elmJson :: Json.Value
elmJson =
    Json.Object $ KM.fromList
        [ ( "type", Json.String "application" )
        , ( "dependencies"
          , Json.Object $ KM.fromList
                [ ( "direct", dependenciesDirect )
                , ( "indirect", dependenciesIndirect )
                ]
          )
        ]


dependenciesDirect :: Json.Value
dependenciesDirect =
    Json.Object $ KM.fromList
        [ ( "elm/browser", Json.String "1.0.2" )
        , ( "elm/core", Json.String "1.0.5" )
        , ( "elm/html", Json.String "1.0.0" )
        , ( "elm/json", Json.String "1.1.3" )
        , ( "elm/url", Json.String "1.0.0" )
        ]


dependenciesIndirect :: Json.Value
dependenciesIndirect =
    Json.Object $ KM.fromList
        [ ( "elm/time", Json.String "1.0.0" )
        , ( "elm/virtual-dom", Json.String "1.0.3" )
        ]


data Dependency
    = Dependency
        { author :: Author
        , package :: Package
        , version :: Version
        }
    deriving Show


type Author = Text
type Package = Text
type Version = Text


dependenciesParser :: String -> Json.Value -> Parser [Dependency]
dependenciesParser name =
    Json.withObject name $ \o ->
        --
        -- 1. Object = KeyMap Value
        -- 2. For each key and value in the KeyMap we want to apply dependencyParser
        -- 3. And collect them into a list, i.e. [Parser Dependency]
        -- 4. Convert [Parser Dependency] to Parser [Dependency] as required
        --
        ifoldl (\key accum value -> dependencyParser key value : accum) [] o
            & sequence


dependencyParser :: Key -> Json.Value -> Parser Dependency
dependencyParser key value = do
    toDependency <$> authorPackageParser key <*> versionParser value
    where
        toDependency :: ( Author, Package ) -> Version -> Dependency
        toDependency ( author, package ) version =
            Dependency author package version


authorPackageParser :: Key -> Parser (Author, Package)
authorPackageParser =
    parse . Text.breakOn "/" . Key.toText
    where
        parse ( author, slashPackage ) =
            case Text.uncons slashPackage of
                Just ( '/', package ) ->
                    if Text.null author then
                        parseFail "author is empty"

                    else if Text.null package then
                        parseFail "package is empty"

                    else
                        pure ( author, package )

                _ ->
                    parseFail "/ is missing"


versionParser :: Json.Value -> Parser Version
versionParser =
    Json.withText "version" $ \t ->
        if isVersion t then
            pure t

        else
            parseFail ("invalid version format: " ++ Text.unpack t)


isVersion :: Text -> Bool
isVersion t =
    --
    -- 1. Must be of the format X.Y.Z
    -- 2. X, Y, and Z must represent natural numbers: 0, 1, 2, etc
    -- 3. Leading zeros are not allowed
    --
    case Text.splitOn "." t of
        [ major, minor, patch ] ->
            isNatural major && isNatural minor && isNatural patch

        _ ->
            False
    where
        isNatural :: Text -> Bool
        isNatural t =
            t == "0" || isNonZeroNatural t

        isNonZeroNatural :: Text -> Bool
        isNonZeroNatural t =
            case Text.uncons t of
                Just ( '0', _ ) ->
                    --
                    -- Say no to leading zeros
                    --
                    False

                Just (  _, rest ) ->
                    Text.all Char.isDigit rest

                Nothing ->
                    False
