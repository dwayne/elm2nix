{-# LANGUAGE OverloadedStrings #-}

module ElmJson where

import qualified Data.Aeson as Json
import qualified Data.Char as Char
import qualified Data.Text as Text

import Data.Aeson.Types (Parser, Value, parseFail)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Text.Read (readMaybe)
import Version (Version(..))


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
