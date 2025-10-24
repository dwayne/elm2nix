{-# LANGUAGE OverloadedStrings #-}

module Sandbox where


import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Char as Char
import qualified Data.Text as Text

import Data.Aeson.Types (Key, Parser, parseEither, parseFail)
import Data.Function ((&))
import Data.Text (Text)


directDependencies :: Json.Object
directDependencies =
    KM.empty
        & KM.insert "elm/browser" (Json.String "1.0.2")
        & KM.insert "elm/core" (Json.String "1.0.5")
        & KM.insert "elm/html" (Json.String "1.0.0")
        & KM.insert "elm/json" (Json.String "1.1.3")
        & KM.insert "elm/url" (Json.String "1.0.0")


type Author = Text
type Package = Text
type Version = Text


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
