module Elm2Nix.Data.Bytes.Decode (Decoder, DecodeError(..), decode) where

import qualified Data.ByteString.Lazy as LBS

import Data.Binary.Get (ByteOffset, Get, runGetOrFail)


type Decoder = Get


data DecodeError
  = Failure
      { unconsumedInput :: LBS.ByteString
      , numBytesConsumed :: ByteOffset
      , message :: String
      }
  deriving (Eq, Show)


decode :: Decoder a -> LBS.ByteString -> Either DecodeError a
decode decoder lbs =
  case runGetOrFail decoder lbs of
    Right (_, _, x) ->
      Right x

    Left (unconsumedInput, numBytesConsumed, message) ->
      Left (Failure unconsumedInput numBytesConsumed message)
