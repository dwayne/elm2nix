module Elm2Nix.Lib.Binary (DecodeFileError(..), decodeFile) where

import qualified Data.Binary as Binary

import Control.Exception (tryJust)
import Control.Monad (join)
import Data.Bifunctor (first)
import Data.Binary (Binary)
import System.IO.Error (isDoesNotExistError)


data DecodeFileError
    = FileNotFound FilePath
    | SyntaxError FilePath String
    deriving (Eq, Show)


decodeFile :: Binary a => FilePath -> IO (Either DecodeFileError a)
decodeFile path =
    join <$> tryJust (handleNotFound . isDoesNotExistError) decodeFileOrFail
    where
        decodeFileOrFail :: Binary a => IO (Either DecodeFileError a)
        decodeFileOrFail =
            first (SyntaxError path . snd) <$> Binary.decodeFileOrFail path

        handleNotFound :: Bool -> Maybe DecodeFileError
        handleNotFound b =
            if b then Just (FileNotFound path) else Nothing
