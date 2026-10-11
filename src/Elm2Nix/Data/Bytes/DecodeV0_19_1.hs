module Elm2Nix.Data.Bytes.DecodeV0_19_1 (w8, w16, w8To16, int, text, list, dict) where

import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE

import Data.Binary.Get (getByteString, getInt64be, getWord8, getWord16be)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word8, Word16)
import Elm2Nix.Data.Bytes.Decode (Decoder)


w8 :: Decoder Word8
w8 =
  getWord8


w16 :: Decoder Word16
w16 =
  getWord16be


w8To16 :: Decoder Word16
w8To16 =
  fmap fromIntegral getWord8


int :: Decoder Int
int =
  fmap fromIntegral getInt64be


text :: Decoder Text
text =
  w8 >>= fmap TE.decodeUtf8 . getByteString . fromIntegral


list :: Decoder a -> Decoder [a]
list elemDecoder =
  int >>= loop
  where
    loop 0 = pure []
    loop n = (:) <$> elemDecoder <*> loop (n - 1)


dict :: Decoder k -> Decoder v -> Decoder (Map k v)
dict keyDecoder valueDecoder =
  Map.fromDistinctAscList <$> list ((,) <$> keyDecoder <*> valueDecoder)
