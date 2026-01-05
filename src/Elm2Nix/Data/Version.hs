{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.Version (Version(..), fromText, decoder) where

import qualified Data.Char as Char
import qualified Data.Text as T

import Control.Applicative (liftA3)
import Control.Monad (liftM3)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Text (Text)
import Data.Word (Word16)

import qualified Elm2Nix.Lib.Json.Decode as JD


data Version
    = Version
        { toMajor :: {-# UNPACK #-} !Word16
        , toMinor :: {-# UNPACK #-} !Word16
        , toPatch :: {-# UNPACK #-} !Word16
        }
    deriving (Eq, Ord)



-- INSTANCES



instance Show Version where
    show (Version major minor patch) =
        show major ++ "." ++ show minor ++ "." ++ show patch


instance Binary Version where
    put (Version major minor patch) =
        if major < 256 && minor < 256 && patch < 256 then do
            putWord8 (fromIntegral major)
            putWord8 (fromIntegral minor)
            putWord8 (fromIntegral patch)
        else do
            putWord8 255
            put major
            put minor
            put patch

    get = do
        word <- getWord8
        if word == 255 then
            liftM3 Version get get get
        else do
            minor <- fmap fromIntegral getWord8
            patch <- fmap fromIntegral getWord8
            return (Version (fromIntegral word) minor patch)



-- CONSTRUCT



fromText :: Text -> Maybe Version
fromText t =
    --
    -- Expected format:
    --
    -- 1. Must be of the form MAJOR.MINOR.PATCH
    -- 2. MAJOR, MINOR, and PATCH must each represent 16-bit unsigned integers: 0, 1, 2, ..., 65535
    -- 3. Leading zeros are not allowed
    --
    case T.splitOn "." t of
        [ x, y, z ] ->
            liftA3 Version (readWord16 x) (readWord16 y) (readWord16 z)

        _ ->
            Nothing


readWord16 :: Text -> Maybe Word16
readWord16 t =
    if t == "0" then
        Just 0

    else
        case T.uncons t of
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
    case T.uncons t of
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


decoder :: JD.Decoder Version
decoder =
    JD.string >>= \s ->
        case fromText (T.pack s) of
            Just version ->
                JD.succeed version

            Nothing ->
                JD.failWith $ "version is invalid: " ++ s
