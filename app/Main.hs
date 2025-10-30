module Main (main) where

import Data.Aeson (Value)

import qualified Elm2Nix.Lib.Json as Json
import qualified Elm2Nix.ElmJson as ElmJson


main :: IO ()
main =
    Json.decodeFile "elm.json" >>= either print (either print print . ElmJson.getDependencies)
