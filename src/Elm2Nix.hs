{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix
    ( writeElmLockFile
    , WriteElmLockFileError(..), writeElmLockFileErrorToText
    , writeRegistryDatFile
    , WriteRegistryDatFileError, writeRegistryDatFileErrorToText
    , viewRegistryDatFile
    , ViewRegistryDatFileError, viewRegistryDatFileErrorToText
    ) where

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.Binary as Binary hiding (decodeFile)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Text as T

import Data.Text (Text)
import System.IO (stdout)

import qualified Elm2Nix.Data.Dependency as Dependency
import qualified Elm2Nix.Data.ElmJson as ElmJson
import qualified Elm2Nix.Data.ElmLock as ElmLock
import qualified Elm2Nix.Data.FixedOutputDerivation as FOD
import qualified Elm2Nix.Data.RegistryDat as RegistryDat
import qualified Elm2Nix.Lib.Binary as Binary
import qualified Elm2Nix.Lib.Json.Decode as JD
import qualified Elm2Nix.Lib.Nix as Nix

import Elm2Nix.Data.FixedOutputDerivation (FixedOutputDerivation)



-- writeElmLockFile



data WriteElmLockFileError
    = FromFilesError (FilePath, JD.Error)
    | FromDependenciesError FOD.FromDependenciesError
    deriving (Eq, Show)


writeElmLockFile :: [FilePath] -> Bool -> FilePath -> IO (Either WriteElmLockFileError ())
writeElmLockFile inputs compact output = do
    result1 <- ElmJson.fromFiles inputs
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
            return $ Left $ FromFilesError err


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


writeElmLockFileErrorToText :: WriteElmLockFileError -> Text
writeElmLockFileErrorToText err =
    case err of
        FromFilesError (path, err) ->
            jsonDecodeFileErrorToText path err

        FromDependenciesError err ->
            fromDependenciesErrorToText err


jsonDecodeFileErrorToText :: FilePath -> JD.Error -> Text
jsonDecodeFileErrorToText path (JD.SyntaxError s) = "Syntax error in " <> T.pack path <> ": " <> T.pack s
jsonDecodeFileErrorToText path (JD.DecodeError err) = "JSON decoding error in " <> T.pack path <> ": " <> jsonDecodeErrorToText err


jsonDecodeErrorToText :: JD.DecodeError -> Text
jsonDecodeErrorToText (JD.Failure s) = T.pack s
jsonDecodeErrorToText (JD.Expected s value) = "Expected " <> T.pack s <> " but got " <> T.pack (show value)
jsonDecodeErrorToText (JD.FieldError name err) = "Problem with field \"" <> T.pack name <> "\": " <> jsonDecodeErrorToText err


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



-- writeRegistryDatFile



type WriteRegistryDatFileError = JD.Error


writeRegistryDatFile :: FilePath -> FilePath -> IO (Either (FilePath, WriteRegistryDatFileError) ())
writeRegistryDatFile input output = do
    result <- ElmLock.fromFile input
    case result of
        Right elmLock ->
            Right <$> Binary.encodeFile output (RegistryDat.fromElmLock elmLock)

        Left err ->
            return $ Left (input, err)


writeRegistryDatFileErrorToText :: FilePath -> WriteRegistryDatFileError -> Text
writeRegistryDatFileErrorToText = jsonDecodeFileErrorToText



-- viewRegistryDatFile



type ViewRegistryDatFileError = Binary.DecodeFileError


viewRegistryDatFile :: Bool -> FilePath -> IO (Either ViewRegistryDatFileError ())
viewRegistryDatFile compact input = do
    result <- Binary.decodeFile input
    case result of
        Right registryDat ->
            let
                allPackages =
                    RegistryDat.toAllPackages registryDat

                ( put, encode ) =
                    if compact then
                        ( LBS.hPut, Json.encode )

                    else
                        ( Char8.hPutStrLn, Json.encodePretty )
            in
            Right <$> put stdout (encode allPackages)

        Left err ->
            return $ Left err


viewRegistryDatFileErrorToText :: ViewRegistryDatFileError -> Text
viewRegistryDatFileErrorToText = binaryDecodeFileErrorToText


binaryDecodeFileErrorToText :: Binary.DecodeFileError -> Text
binaryDecodeFileErrorToText err =
    case err of
        Binary.FileNotFound path ->
            "File not found: " <> T.pack path

        Binary.DecodeError path details ->
            "Syntax error in " <> T.pack path <> ": " <> T.pack details
