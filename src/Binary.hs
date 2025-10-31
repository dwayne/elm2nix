module Binary where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.Binary as Binary

import Data.Binary.Put (Put, putBuilder, putWord8, runPut)
import Data.Word (Word8)
import GHC.IsList (toList)


viewPut :: Put -> [Word8]
viewPut =
    toList . runPut


putUnder256 :: BS.ByteString -> Put
putUnder256 bs = do
    putWord8 (fromIntegral (BS.length bs))
    putBuilder (Builder.byteString bs)
