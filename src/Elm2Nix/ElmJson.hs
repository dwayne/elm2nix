{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.ElmJson (getDependencies) where

import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text

import Data.Aeson.Types ((<?>), (.:?), (.!=), JSONPathElement(..), Key, Object, Parser, Value, parseEither, parseFail)
import Data.Foldable.WithIndex (ifoldl)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String (fromString)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Text.Read (readMaybe)

import Elm2Nix.Data.Dependency (Author, Dependency(..), Package)
import Elm2Nix.Data.Version (Version(..))


getDependencies :: Value -> Either String (Set Dependency)
getDependencies =
    parseEither elmDotJsonParser



elmDotJsonParser :: Value -> Parser (Set Dependency)
elmDotJsonParser =
    Json.withObject "elm.json" $ \o ->
        Set.union <$> withDirectAndIndirect "dependencies" o <*> withDirectAndIndirect "test-dependencies" o


withDirectAndIndirect :: Key -> Object -> Parser (Set Dependency)
withDirectAndIndirect key parent =
    let
        name =
            Key.toString key
    in
    ((parent .:? key .!= emptyObject) >>= Json.withObject name (\o ->
        Set.union
            <$> ((o .:? "direct" .!= emptyObject) >>= (\v -> dependenciesParser "direct" v <?> Key key))
            <*> ((o .:? "indirect" .!= emptyObject) >>= (\v -> dependenciesParser "indirect" v <?> Key key))
    ))


emptyObject :: Value
emptyObject =
    Json.Object KeyMap.empty


dependenciesParser :: String -> Value -> Parser (Set Dependency)
dependenciesParser name =
    Json.withObject name reduceWithContext
    where
        reduceWithContext :: Object -> Parser (Set Dependency)
        reduceWithContext o =
            reduce o <?> Key (fromString name)

        reduce :: Object -> Parser (Set Dependency)
        reduce =
            fmap Set.fromList
            . sequence
            . ifoldl (\k ds v -> dependencyParser k v <?> Key k : ds) []


dependencyParser :: Key -> Value -> Parser Dependency
dependencyParser key value =
    toDependency <$> authorPackageParser key <*> versionParser value
    where
        toDependency :: ( Author, Package ) -> Version -> Dependency
        toDependency ( author, package ) =
            Dependency author package


authorPackageParser :: Key -> Parser (Author, Package)
authorPackageParser =
    --
    -- Expected format:
    --
    -- 1. author/package
    -- 2. author must be non-empty
    -- 3. package must be non-empty
    --
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


versionParser :: Value -> Parser Version
versionParser =
    Json.withText "version" $ either parseFail pure . versionFromText


versionFromText :: Text -> Either String Version
versionFromText t =
    --
    -- Expected format:
    --
    -- 1. Must be of the form X.Y.Z
    -- 2. X, Y, and Z must represent natural numbers: 0, 1, 2, etc
    -- 3. Leading zeros are not allowed
    --
    case Text.splitOn "." t of
        [ x, y, z ] | isNatural x && isNatural y && isNatural z ->
            Right $ Version (readNatural x) (readNatural y) (readNatural z)

        _ ->
            Left $ "version is incorrectly formatted: " ++ Text.unpack t

    where
        isNatural :: Text -> Bool
        isNatural t =
            t == "0" || isNonZeroNatural t

        isNonZeroNatural :: Text -> Bool
        isNonZeroNatural t =
            case Text.uncons t of
                Just ( '0', _ ) ->
                    --
                    -- No leading zeros allowed
                    --
                    False

                Just (  x, ts ) ->
                    --
                    -- It starts with a non-zero digit and is followed by digits
                    --
                    Char.isDigit x && Text.all Char.isDigit ts

                Nothing ->
                    --
                    -- Must be non-empty
                    --
                    False


        readNatural :: Text -> Natural
        readNatural =
            fromMaybe 0 . readMaybe . Text.unpack
