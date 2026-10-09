module Elm2Nix.Data.Bytes.DecodeV0_19_1
  ( Decoder
  , w8, w16, w8To16
  , int
  , text
  , list
  , dict
  , decode
  ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE

import Control.Applicative (empty)
import Data.Binary.Get (Get, getByteString, getInt64be, getWord8, getWord16be, runGet)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word8, Word16)


type Decoder = Get


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
    loop 0 = empty
    loop n = (:) <$> elemDecoder <*> loop (n - 1)


dict :: Decoder k -> Decoder v -> Decoder (Map k v)
dict keyDecoder valueDecoder =
  int >>= fmap Map.fromDistinctAscList . loop
  where
    loop 0 = empty
    loop n = (\k v rest -> (k, v) : rest) <$> keyDecoder <*> valueDecoder <*> loop (n - 1)


decode :: Decoder a -> LBS.ByteString -> a
decode = runGet
