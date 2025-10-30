{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy as LBS
import qualified Elm2Nix.Data.FixedOutputDerivation as FOD


main :: IO ()
main =
    FOD.fromFile "elm.json" >>= either print (LBS.writeFile "elm.lock" . Json.encodePretty' config)


config :: Json.Config
config =
    Json.defConfig
        { Json.confCompare = Json.keyOrder [ "author", "package", "version", "sha256" ]
        , Json.confTrailingNewline = True
        }
