module Main (main) where

import qualified Data.Aeson as Json
import Data.Aeson (Value)

main :: IO ()
main = do
    result <- decodeFile "elm.json"

    case result of
        Right value ->
            putStrLn (show value)

        Left err ->
            putStrLn ("An error occurred: " ++ err)


decodeFile :: FilePath -> IO (Either String Value)
decodeFile =
    Json.eitherDecodeFileStrict
