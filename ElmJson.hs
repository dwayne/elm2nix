{-# LANGUAGE OverloadedStrings #-}

module ElmJson where

import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Char as Char
import qualified Data.Text as Text

import Data.Aeson.Types (Key, Parser, Value, parseFail)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Text.Read (readMaybe)

import Dependency (Author, Dependency(..), Package)
import Version (Version(..))


dependencyParser :: Key -> Value -> Parser Dependency
dependencyParser key value =
    toDependency <$> authorPackageParser key <*> versionParser value
    where
        toDependency :: ( Author, Package ) -> Version -> Dependency
        toDependency ( author, package ) version =
            Dependency author package version


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
            maybe 0 id . readMaybe . Text.unpack
