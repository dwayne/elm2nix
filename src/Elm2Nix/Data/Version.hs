{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.Version
  ( Version(..)
  , fromText
  , jsonDecoder
  , binaryEncoderV0_19_1, binaryDecoderV0_19_1
  ) where

import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Elm2Nix.Data.Bytes.DecodeV0_19_1 as DV0_19_1
import qualified Elm2Nix.Data.Bytes.EncodeV0_19_1 as EV0_19_1
import qualified Json.Decode as JD

import Control.Applicative (liftA3)
import Data.Text (Text)
import Data.Word (Word16)
import Json.Decode (FromJson)


data Version
  = Version
      { toMajor :: {-# UNPACK #-} !Word16
      , toMinor :: {-# UNPACK #-} !Word16
      , toPatch :: {-# UNPACK #-} !Word16
      }
  deriving (Eq, Ord)



-- Instances



instance Show Version where
  show (Version major minor patch) =
    show major ++ "." ++ show minor ++ "." ++ show patch


instance FromJson Version where
  decoder = jsonDecoder



-- Construct



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



-- JSON Decoder



jsonDecoder :: JD.Decoder Version
jsonDecoder =
  JD.text >>= \t ->
    case fromText t of
      Just version ->
        JD.succeed version

      Nothing ->
        JD.fail $ "version is invalid: " <> t



-- Binary Encoder/Decoder for Elm 0.19.1



binaryEncoderV0_19_1 :: Version -> EV0_19_1.Encoder
binaryEncoderV0_19_1 (Version major minor patch) =
  if major < 256 && minor < 256 && patch < 256 then
    EV0_19_1.w16To8 major <> EV0_19_1.w16To8 minor <> EV0_19_1.w16To8 patch

  else
    EV0_19_1.w8 255 <> EV0_19_1.w16 major <> EV0_19_1.w16 minor <> EV0_19_1.w16 patch


binaryDecoderV0_19_1 :: DV0_19_1.Decoder Version
binaryDecoderV0_19_1 = do
  major <- DV0_19_1.w8To16
  if major == 255 then
    Version <$> DV0_19_1.w16 <*> DV0_19_1.w16 <*> DV0_19_1.w16
  else
    Version major <$> DV0_19_1.w8To16 <*> DV0_19_1.w8To16
