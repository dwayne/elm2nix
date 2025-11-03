{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.Version (Version(..), fromText) where

import qualified Data.Char as Char
import qualified Data.Text as Text

import Control.Applicative (liftA3)
import Data.Text (Text)
import Data.Word (Word16)


data Version
    = Version
        { major :: {-# UNPACK #-} !Word16
        , minor :: {-# UNPACK #-} !Word16
        , patch :: {-# UNPACK #-} !Word16
        }
    deriving (Eq, Ord)


instance Show Version where
    show (Version major minor patch) =
        show major ++ "." ++ show minor ++ "." ++ show patch


fromText :: Text -> Maybe Version
fromText t =
    --
    -- Expected format:
    --
    -- 1. Must be of the form MAJOR.MINOR.PATCH
    -- 2. MAJOR, MINOR, and PATCH must each represent 16-bit unsigned integers: 0, 1, 2, ..., 65535
    -- 3. Leading zeros are not allowed
    --
    case Text.splitOn "." t of
        [ x, y, z ] ->
            liftA3 Version (readWord16 x) (readWord16 y) (readWord16 z)

        _ ->
            Nothing


readWord16 :: Text -> Maybe Word16
readWord16 t =
    if t == "0" then
        Just 0

    else
        case Text.uncons t of
            Just (  d, s ) | d /= '0' && Char.isDigit d ->
                --
                -- It starts with a non-zero digit
                --
                readWord16Helper (Char.digitToInt d) s

            _ ->
                --
                -- It must be non-empty and the leading character must be a non-zero digit
                --
                Nothing


readWord16Helper :: Int -> Text -> Maybe Word16
readWord16Helper n t =
    case Text.uncons t of
        Just ( d, s ) ->
            if Char.isDigit d then
                let
                    m = n * 10 + Char.digitToInt d
                in
                if m <= maxWord16 then
                    readWord16Helper m s

                else
                    Nothing

            else
                Nothing

        Nothing ->
            Just $ fromIntegral n


maxWord16 :: Int
maxWord16 =
    fromIntegral (maxBound :: Word16)
