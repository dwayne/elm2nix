{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Cli.Error (Error(..), toText) where

import qualified Data.Text as T
import qualified Elm2Nix.Command.ViewRegistryDatFile as ViewRegistryDatFile
import qualified Elm2Nix.Command.WriteElmLockFile as WriteElmLockFile
import qualified Elm2Nix.Command.WriteRegistryDatFile as WriteRegistryDatFile
import qualified Elm2Nix.Data.Dependency as Dependency
import qualified Elm2Nix.Data.FixedOutputDerivation as FOD
import qualified Elm2Nix.Data.RegistryDat.Decode as RDD
import qualified Elm2Nix.Lib.NixPrefetchUrl as NixPrefetchUrl
import qualified Json.Decode as JD

import Data.Text (Text)


data Error
  = WriteElmLockFileError WriteElmLockFile.Error
  | WriteRegistryDatFileError WriteRegistryDatFile.Error
  | ViewRegistryDatFileError ViewRegistryDatFile.Error
  deriving (Eq, Show)


toText :: Error -> Text
toText err =
  case err of
    WriteElmLockFileError wrappedErr ->
      writeElmLockFileErrorToText wrappedErr

    WriteRegistryDatFileError wrappedErr ->
      writeRegistryDatFileErrorToText wrappedErr

    ViewRegistryDatFileError wrappedErr ->
      viewRegistryDatFileErrorToText wrappedErr


writeElmLockFileErrorToText :: WriteElmLockFile.Error -> Text
writeElmLockFileErrorToText err =
  case err of
    WriteElmLockFile.FromFilesError path err ->
      jsonDecodeFileErrorToText path err

    WriteElmLockFile.FromDependenciesError err ->
      fromDependenciesErrorToText err


fromDependenciesErrorToText :: FOD.FromDependenciesError -> Text
fromDependenciesErrorToText =
  T.unlines . map (\(d, err) -> T.pack (Dependency.toString d) <> ": " <> nixPrefetchUrlErrorToText err)


nixPrefetchUrlErrorToText :: NixPrefetchUrl.Error -> Text
nixPrefetchUrlErrorToText err =
  case err of
    NixPrefetchUrl.ProcessError details ->
      "nix-prefetch-url encountered problems: " <> T.pack details

    NixPrefetchUrl.BadOutput details ->
      "nix-prefetch-url got unexpected output: " <> T.pack details


writeRegistryDatFileErrorToText :: WriteRegistryDatFile.Error -> Text
writeRegistryDatFileErrorToText err =
  case err of
    WriteRegistryDatFile.JsonDecodeError path jsonDecodeError ->
      jsonDecodeFileErrorToText path jsonDecodeError


jsonDecodeFileErrorToText :: FilePath -> JD.Error -> Text
--
-- TODO: Improve error messages.
--
jsonDecodeFileErrorToText path (JD.EncodingError u) = "Unicode error in " <> T.pack path <> ": " <> T.show u
jsonDecodeFileErrorToText path (JD.SyntaxError s) = "Syntax error in " <> T.pack path <> ": " <> T.show s
jsonDecodeFileErrorToText path (JD.DecodeError err) = "JSON decoding error in " <> T.pack path <> ": " <> jsonDecodeErrorToText err


jsonDecodeErrorToText :: JD.DecodeError -> Text
jsonDecodeErrorToText =
  --
  -- TODO: Improve error messages.
  --
  T.show


viewRegistryDatFileErrorToText :: ViewRegistryDatFile.Error -> Text
viewRegistryDatFileErrorToText err =
  case err of
    ViewRegistryDatFile.FileNotFound path ->
      "File not found: " <> T.pack path

    ViewRegistryDatFile.DecodeError path decodeError ->
      case decodeError of
        RDD.Failure _ _ message ->
          "Corrupted bytes in " <> T.pack path <> ": " <> T.pack message
