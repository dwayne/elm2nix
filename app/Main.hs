module Main (main) where

import qualified Data.Aeson as Json
import qualified Elm2Nix.Data.FixedOutputDerivation as FOD


main :: IO ()
main =
    FOD.fromFile "elm.json" >>= either print (Json.encodeFile "elm.lock")
