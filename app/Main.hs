module Main (main) where

import qualified Data.Text.IO as TIO

import Data.Text (Text)
import System.Exit (exitFailure)
import System.IO (stderr)

import qualified Elm2Nix
import qualified Elm2Nix.CLI as CLI


main :: IO ()
main = do
    --
    -- TODO: Handle unexpected exceptions
    --
    cli <- CLI.runIO
    case cli of
        CLI.Lock (CLI.LockOptions compact input output) ->
            Elm2Nix.writeElmLockFile compact input output >>= either (die . Elm2Nix.writeElmLockFileErrorToText) return

        CLI.Registry (CLI.Generate (CLI.GenerateOptions input output)) ->
            Elm2Nix.writeRegistryDatFile input output >>= either (die . Elm2Nix.writeRegistryDatFileErrorToText) return

        CLI.Registry (CLI.View (CLI.ViewOptions input)) ->
            --
            -- TODO: Add a compact option
            --
            Elm2Nix.viewRegistryDatFile False input >>= either (die . Elm2Nix.viewRegistryDatFileErrorToText) return


die :: Text -> IO ()
die t = TIO.hPutStrLn stderr t >> exitFailure
