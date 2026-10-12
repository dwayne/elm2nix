{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Elm2Nix.Command.ViewRegistryDatFile as ViewRegistryDatFile
import qualified Elm2Nix.Command.WriteElmLockFile as WriteElmLockFile
import qualified Elm2Nix.Command.WriteRegistryDatFile as WriteRegistryDatFile
import qualified Elm2Nix.CLI as CLI
import qualified Elm2Nix.Data.Bytes.Decode as BD
import qualified Elm2Nix.Data.Dependency as Dependency
import qualified Elm2Nix.Data.FixedOutputDerivation as FOD
import qualified Elm2Nix.Lib.Nix as Nix
import qualified Json.Decode as JD

import Data.Text (Text)
import System.Exit (ExitCode, exitFailure)
import System.IO (hPutStrLn, stderr)


main :: IO ()
main = runSafely unsafeMain


unsafeMain :: IO ()
unsafeMain = do
  cli <- CLI.runIO
  case cli of
    CLI.Lock (CLI.LockOptions inFiles compact outFiles) ->
      WriteElmLockFile.run inFiles compact outFiles >>= either (die . writeElmLockFileErrorToText) return

    CLI.Registry (CLI.Generate (CLI.GenerateOptions elmVersion inFile outFile)) ->
      WriteRegistryDatFile.run elmVersion inFile outFile >>= either (die . writeRegistryDatFileErrorToText) return

    CLI.Registry (CLI.View (CLI.ViewOptions elmVersion compact inFile)) ->
      ViewRegistryDatFile.run elmVersion compact inFile >>= either (die . viewRegistryDatFileErrorToText) return


die :: Text -> IO ()
die t = TIO.hPutStrLn stderr t >> exitFailure


runSafely :: IO () -> IO ()
runSafely action =
  action `E.catches`
    [ E.Handler rethrowAsync
    , E.Handler rethrowExitCode
    , E.Handler handleUnexpected
    ]
  where
    rethrowAsync :: E.AsyncException -> IO ()
    rethrowAsync = E.throwIO

    rethrowExitCode :: ExitCode -> IO ()
    rethrowExitCode = E.throwIO

    handleUnexpected :: E.SomeException -> IO ()
    handleUnexpected e =
      hPutStrLn stderr ("An unexpected error occurred: " ++ E.displayException e) >> exitFailure


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


nixPrefetchUrlErrorToText :: Nix.NixPrefetchUrlError -> Text
nixPrefetchUrlErrorToText err =
  case err of
    Nix.ProcessError details ->
      "nix-prefetch-url encountered problems: " <> T.pack details

    Nix.BadOutput details ->
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
        BD.Failure _ _ message ->
          "Corrupted bytes in " <> T.pack path <> ": " <> T.pack message
