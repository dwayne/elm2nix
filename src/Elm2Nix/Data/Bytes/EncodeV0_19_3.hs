module Elm2Nix.Data.Bytes.EncodeV0_19_3 (u8, u16, int, string8, list32, dict32) where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE

import Data.Binary.Builder (fromByteString, putInt64le, putWord16le, putWord32le, singleton)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word8, Word16, Word32)
import Elm2Nix.Data.Bytes.Encode (Encoder)


u8 :: Word8 -> Encoder
u8 = singleton


u16 :: Word16 -> Encoder
u16 = putWord16le


u32 :: Word32 -> Encoder
u32 = putWord32le


int :: Int -> Encoder
int = putInt64le . fromIntegral


string8 :: Text -> Encoder
string8 t =
  u8 (fromIntegral $ BS.length bs) <> fromByteString bs
  where
    bs = TE.encodeUtf8 t


list32 :: (a -> Encoder) -> [a] -> Encoder
list32 encodeElem xs =
  n <> elems
  where
    n     = u32 (fromIntegral $ length xs)
    elems = foldMap encodeElem xs


dict32 :: (k -> Encoder) -> (v -> Encoder) -> Map k v -> Encoder
dict32 encodeKey encodeValue m =
  n <> elems
  where
    n     = u32 (fromIntegral $ Map.size m)
    elems = Map.foldrWithKey (\k v b -> encodeKey k <> encodeValue v <> b) mempty m
