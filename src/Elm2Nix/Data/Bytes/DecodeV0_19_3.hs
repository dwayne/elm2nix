module Elm2Nix.Data.Bytes.DecodeV0_19_3 (u8, u16, int, string8, list32, dict32) where

import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE

import Data.Binary.Get (getByteString, getInt64le, getWord8, getWord16le, getWord32le)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word8, Word16, Word32)
import Elm2Nix.Data.Bytes.Decode (Decoder)


u8 :: Decoder Word8
u8 = getWord8


u16 :: Decoder Word16
u16 = getWord16le


u32 :: Decoder Word32
u32 = getWord32le


int :: Decoder Int
int =
  fmap fromIntegral getInt64le


string8 :: Decoder Text
string8 =
  u8 >>= fmap TE.decodeUtf8 . getByteString . fromIntegral


list32 :: Decoder a -> Decoder [a]
list32 elemDecoder =
  u32 >>= loop
  where
    loop 0 = pure []
    loop n = (:) <$> elemDecoder <*> loop (n - 1)


dict32 :: Decoder k -> Decoder v -> Decoder (Map k v)
dict32 keyDecoder valueDecoder =
  Map.fromDistinctAscList <$> list32 ((,) <$> keyDecoder <*> valueDecoder)
