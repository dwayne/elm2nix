module Elm2Nix.Lib.Json
    ( DecodeFileError(..)
    , decodeFile
    ) where

import qualified Data.Aeson as Json

import Control.Exception (tryJust)
import Control.Monad (join)
import Data.Aeson (FromJSON)
import Data.Bifunctor (first)
import System.IO.Error (isDoesNotExistError)


data DecodeFileError
    = FileNotFound FilePath
    | SyntaxError String
    deriving (Eq, Show)


decodeFile :: FromJSON a => FilePath -> IO (Either DecodeFileError a)
decodeFile path =
    fmap join
        $ tryJust (handleNotFound . isDoesNotExistError)
        $ eitherDecodeFileStrict path
    where
        eitherDecodeFileStrict :: FromJSON a => FilePath -> IO (Either DecodeFileError a)
        eitherDecodeFileStrict =
            fmap (first SyntaxError) . Json.eitherDecodeFileStrict

        handleNotFound :: Bool -> Maybe DecodeFileError
        handleNotFound b =
            if b then Just (FileNotFound path) else Nothing
