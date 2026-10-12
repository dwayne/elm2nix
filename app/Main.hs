{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Elm2Nix
import qualified Elm2Nix.Data.Bytes.Decode as BD
import qualified Elm2Nix.Command.ViewRegistryDatFile as ViewRegistryDatFile
import qualified Elm2Nix.CLI as CLI

import Data.Text (Text)
import System.Exit (ExitCode, exitFailure)
import System.IO (hPutStrLn, stderr)


main :: IO ()
main = runSafely unsafeMain


unsafeMain :: IO ()
unsafeMain = do
  cli <- CLI.runIO
  case cli of
    CLI.Lock (CLI.LockOptions inputs compact output) ->
      Elm2Nix.writeElmLockFile inputs compact output >>= either (die . Elm2Nix.writeElmLockFileErrorToText) return

    CLI.Registry (CLI.Generate (CLI.GenerateOptions input output)) ->
      Elm2Nix.writeRegistryDatFile input output >>= either (die . uncurry Elm2Nix.writeRegistryDatFileErrorToText) return

    CLI.Registry (CLI.View (CLI.ViewOptions elmVersion compact input)) ->
      ViewRegistryDatFile.run (ViewRegistryDatFile.Input elmVersion compact input) >>= either (die . viewRegistryDatFileErrorToText) return


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


viewRegistryDatFileErrorToText :: ViewRegistryDatFile.Error -> Text
viewRegistryDatFileErrorToText err =
  case err of
    ViewRegistryDatFile.FileNotFound path ->
      "File not found: " <> T.pack path

    ViewRegistryDatFile.DecodeError path decodeError ->
      case decodeError of
        BD.Failure _ _ message ->
          "Corrupted bytes in " <> T.pack path <> ": " <> T.pack message
