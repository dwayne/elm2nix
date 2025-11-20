module Main (main) where

import qualified Control.Exception as E
import qualified Data.Text.IO as TIO

import Data.Text (Text)
import System.Exit (ExitCode, exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Elm2Nix
import qualified Elm2Nix.CLI as CLI


main :: IO ()
main = runSafely unsafeMain


unsafeMain :: IO ()
unsafeMain = do
    cli <- CLI.runIO
    case cli of
        CLI.Lock (CLI.LockOptions compact input output) ->
            Elm2Nix.writeElmLockFile compact input output >>= either (die . Elm2Nix.writeElmLockFileErrorToText) return

        CLI.Registry (CLI.Generate (CLI.GenerateOptions input output)) ->
            Elm2Nix.writeRegistryDatFile input output >>= either (die . Elm2Nix.writeRegistryDatFileErrorToText) return

        CLI.Registry (CLI.View (CLI.ViewOptions compact input)) ->
            Elm2Nix.viewRegistryDatFile compact input >>= either (die . Elm2Nix.viewRegistryDatFileErrorToText) return


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
