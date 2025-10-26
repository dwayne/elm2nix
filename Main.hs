module Main (main) where

import qualified Data.Aeson as Json

import Control.Exception (tryJust)
import Control.Monad (join)
import Data.Aeson (Value)
import Data.Bifunctor (first)
import System.IO.Error (isDoesNotExistError)


main :: IO ()
main =
    decodeFile "elm.json" >>= either print print


data DecodeFileError
    = FileNotFound FilePath
    | JsonError String
    deriving Show


decodeFile :: FilePath -> IO (Either DecodeFileError Value)
decodeFile path =
    fmap join
        $ tryJust (handleNotFound . isDoesNotExistError)
        $ eitherDecodeFileStrict path
    where
        eitherDecodeFileStrict :: FilePath -> IO (Either DecodeFileError Value)
        eitherDecodeFileStrict =
            fmap (first JsonError) . Json.eitherDecodeFileStrict

        handleNotFound :: Bool -> Maybe DecodeFileError
        handleNotFound b =
            if b then Just (FileNotFound path) else Nothing
