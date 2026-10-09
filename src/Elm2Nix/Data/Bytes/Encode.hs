module Elm2Nix.Data.Bytes.Encode (Encoder, encode) where

import qualified Data.ByteString.Lazy as LBS

import Data.Binary.Builder (Builder, toLazyByteString)


type Encoder = Builder


encode :: Encoder -> LBS.ByteString
encode = toLazyByteString
