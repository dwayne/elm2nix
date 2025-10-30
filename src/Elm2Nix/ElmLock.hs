{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.ElmLock (GenerateLockFileError, generateLockFile) where

import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy as LBS
import qualified Elm2Nix.Data.FixedOutputDerivation as FOD


type GenerateLockFileError = FOD.FromFileError


generateLockFile :: FilePath -> FilePath -> IO (Either GenerateLockFileError ())
generateLockFile inputFilePath outputFilePath =
    --
    -- TODO: Consider how LBS.writeFile could fail.
    --
    FOD.fromFile inputFilePath >>= either (return . Left) (fmap Right . LBS.writeFile outputFilePath . Json.encodePretty' config)


config :: Json.Config
config =
    Json.defConfig
        { Json.confCompare = Json.keyOrder [ "author", "package", "version", "sha256" ]
        , Json.confTrailingNewline = True
        }
