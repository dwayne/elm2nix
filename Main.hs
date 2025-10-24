module Main (main) where

import qualified Data.Aeson as Json

import Control.Exception (tryJust)
import Control.Monad (join)
import Data.Aeson (Value)
import Data.Bifunctor (first)
import System.IO.Error (isDoesNotExistError)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as Text

import Data.Aeson.Key (Key)
import Data.Aeson.Types (Parser)
import Data.Traversable.WithIndex (ifor)


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


-- dependenciesParser :: String -> Value -> Parser [(String, String)]
-- dependenciesParser name =
--     Json.withObject name $ \o ->
--         -- So we have an object which is a key map
--         -- We want to create a parser for each (Key, Value) pair in the object
--         -- Then we want to combine that into a parser for the entire object
--         -- So we're folding over the object
--         fmap KM.toList (ifor o dependencyParser)
--         -- ifor o dependencyParser :: Parser (KeyMap (String, String))


dependencyParser :: Key -> Value -> Parser (String, String)
dependencyParser key value =
    (,) <$> pure (Key.toString key) <*> versionParser value


versionParser :: Value -> Parser String
versionParser =
    Json.withText "version" (pure . Text.unpack)
