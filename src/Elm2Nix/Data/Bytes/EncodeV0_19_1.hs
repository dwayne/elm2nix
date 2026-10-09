module Elm2Nix.Data.Bytes.EncodeV0_19_1 (w8, w16, w16To8, int, text, list, dict) where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE

import Data.Binary.Builder (fromByteString, putInt64be, putWord16be, singleton)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word8, Word16)
import Elm2Nix.Data.Bytes.Encode (Encoder)


w8 :: Word8 -> Encoder
w8 = singleton


w16 :: Word16 -> Encoder
w16 = putWord16be


w16To8 :: Word16 -> Encoder
w16To8 = singleton . fromIntegral


int :: Int -> Encoder
int = putInt64be . fromIntegral


text :: Text -> Encoder
text t =
  w8 (fromIntegral $ BS.length bs) <> fromByteString bs
  where
    bs = TE.encodeUtf8 t


list :: (a -> Encoder) -> [a] -> Encoder
list encodeElem xs =
  n <> elems
  where
    n     = int $ length xs
    elems = foldMap encodeElem xs


dict :: (k -> Encoder) -> (v -> Encoder) -> Map k v -> Encoder
dict encodeKey encodeValue m =
  n <> elems
  where
    n     = int $ Map.size m
    elems = foldMap (\(k, v) -> encodeKey k <> encodeValue v) (Map.toAscList m)
