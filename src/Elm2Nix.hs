{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix
  ( writeElmLockFile
  , WriteElmLockFileError(..), writeElmLockFileErrorToText
  , writeRegistryDatFile
  , WriteRegistryDatFileError, writeRegistryDatFileErrorToText
  , viewRegistryDatFile
  , ViewRegistryDatFileError, viewRegistryDatFileErrorToText
  ) where

import qualified Data.Binary as Binary hiding (decodeFile)
import qualified Data.Json as Json
import qualified Data.Json.Decode as JD
import qualified Data.Json.Encode as JE
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Elm2Nix.Data.Dependency as Dependency
import qualified Elm2Nix.Data.ElmJson as ElmJson
import qualified Elm2Nix.Data.ElmLock as ElmLock
import qualified Elm2Nix.Data.FixedOutputDerivation as FOD
import qualified Elm2Nix.Data.RegistryDat as RegistryDat
import qualified Elm2Nix.Lib.Binary as Binary
import qualified Elm2Nix.Lib.Nix as Nix

import Data.Json (Json)
import Data.Text (Text)
import Elm2Nix.Data.FixedOutputDerivation (FixedOutputDerivation)
import Elm2Nix.Data.RegistryDat (RegistryDat)
import System.IO (stdout)



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
encodeCompact output = Json.writeCompact output . JE.encode


encodeExpanded :: FilePath -> [FixedOutputDerivation] -> IO ()
encodeExpanded output =
  Json.writePretty output 4 . JE.encode


writeElmLockFileErrorToText :: WriteElmLockFileError -> Text
writeElmLockFileErrorToText err =
  case err of
    FromFilesError (path, err) ->
      jsonDecodeFileErrorToText path err

    FromDependenciesError err ->
      fromDependenciesErrorToText err


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
        ( put, toText ) =
          if compact then
            ( TIO.hPutStr, Json.compact )

          else
            ( TIO.hPutStrLn, Json.pretty 4 )

        encode :: RegistryDat -> Json
        encode = JE.encode
      in
      Right <$> put stdout (toText $ encode registryDat)

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
