{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix
    ( WriteElmLockFileError(..), writeElmLockFile
    , WriteRegistryDatFileError, writeRegistryDatFile
    , writeElmLockFileErrorToText, writeRegistryDatFileErrorToText
    ) where

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import Data.Text (Text)

import qualified Elm2Nix.Data.Dependency as Dependency
import qualified Elm2Nix.Data.FixedOutputDerivation as FOD
import qualified Elm2Nix.Data.RegistryDat as RegistryDat
import qualified Elm2Nix.Lib.Json as Json
import qualified Elm2Nix.Lib.Nix as Nix

import Elm2Nix.Data.FixedOutputDerivation (FixedOutputDerivation)


data WriteElmLockFileError
    = DecodeFileError Json.DecodeFileError
    | FromDependenciesError FOD.FromDependenciesError
    deriving (Eq, Show)


writeElmLockFile :: Bool -> FilePath -> FilePath -> IO (Either WriteElmLockFileError ())
writeElmLockFile compact input output = do
    result1 <- Json.decodeFile input
    case result1 of
        Right elmJson -> do
            result2 <- FOD.fromElmJson elmJson
            case result2 of
                Right fods ->
                    fmap Right $
                        if compact then
                            encodeCompact output fods

                        else
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


writeElmLockFileErrorToText :: WriteElmLockFileError -> Text
writeElmLockFileErrorToText err =
    case err of
        DecodeFileError err ->
            decodeFileErrorToText err

        FromDependenciesError err ->
            fromDependenciesErrorToText err


writeRegistryDatFileErrorToText :: WriteRegistryDatFileError -> Text
writeRegistryDatFileErrorToText = decodeFileErrorToText


decodeFileErrorToText :: Json.DecodeFileError -> Text
decodeFileErrorToText err =
    case err of
        Json.FileNotFound path ->
            "File not found: " <> T.pack path

        Json.SyntaxError path details ->
            "Syntax error in " <> T.pack path <> ": " <> T.pack details


fromDependenciesErrorToText :: FOD.FromDependenciesError -> Text
fromDependenciesErrorToText =
    T.unlines . map (\(d, err) -> T.pack (Dependency.toString d) <> ": " <> nixPrefetchUrlErrorToText err)


nixPrefetchUrlErrorToText :: Nix.NixPrefetchUrlError -> Text
nixPrefetchUrlErrorToText err =
    case err of
        Nix.ProcessError details ->
            "nix-prefetch-url encountered problems: " <> T.pack details

        Nix.BadOutput details ->
            "nix-prefetch-url got unexpected output: " <> T.pack details
