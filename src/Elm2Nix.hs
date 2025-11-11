{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix
    ( Format(..), WriteElmLockFileError(..), writeElmLockFile
    , WriteRegistryDatFileError, writeRegistryDatFile
    ) where

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS

import qualified Elm2Nix.Data.FixedOutputDerivation as FOD
import qualified Elm2Nix.Data.RegistryDat as RegistryDat
import qualified Elm2Nix.Lib.Json as Json

import Elm2Nix.Data.FixedOutputDerivation (FixedOutputDerivation)


data Format
    = Compact
    | Expanded
    deriving (Eq, Show)


data WriteElmLockFileError
    = DecodeFileError Json.DecodeFileError
    | FromDependenciesError FOD.FromDependenciesError
    deriving (Eq, Show)


writeElmLockFile :: Format -> FilePath -> FilePath -> IO (Either WriteElmLockFileError ())
writeElmLockFile format input output = do
    result1 <- Json.decodeFile input
    case result1 of
        Right elmJson -> do
            result2 <- FOD.fromElmJson elmJson
            case result2 of
                Right fods ->
                    fmap Right $
                        case format of
                            Compact ->
                                encodeCompact output fods

                            Expanded ->
                                encodeExpanded output fods

                Left err ->
                    return $ Left $ FromDependenciesError err

        Left err ->
            return $ Left $ DecodeFileError err


encodeCompact :: FilePath -> [FixedOutputDerivation] -> IO ()
encodeCompact = Json.encodeFile


encodeExpanded :: FilePath -> [FixedOutputDerivation] -> IO ()
encodeExpanded output =
    LBS.writeFile output . Json.encodePretty' config
    where
        config :: Json.Config
        config =
            Json.defConfig
                { Json.confCompare = Json.keyOrder [ "author", "package", "version", "sha256" ]
                , Json.confTrailingNewline = True
                }


type WriteRegistryDatFileError = Json.DecodeFileError


writeRegistryDatFile :: FilePath -> FilePath -> IO (Either WriteRegistryDatFileError ())
writeRegistryDatFile input output = do
    result <- Json.decodeFile input
    case result of
        Right elmJson ->
            Right <$> Binary.encodeFile output (RegistryDat.fromElmJson elmJson)

        Left err ->
            return $ Left err
