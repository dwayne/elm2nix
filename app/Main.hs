module Main (main) where

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
            let
                format =
                    if compact then
                        Elm2Nix.Compact

                    else
                        Elm2Nix.Expanded
            in
            --
            -- TODO:
            --
            -- - Fail with non-zero error code
            -- - Display human-readable error messages
            --
            Elm2Nix.writeElmLockFile format input output >>= either print (const $ return ())

        CLI.Registry (CLI.Generate (CLI.GenerateOptions input output)) ->
            --
            -- TODO: Implement generate
            --
            print input >> print output

        CLI.Registry (CLI.View (CLI.ViewOptions input)) ->
            --
            -- TODO: Implement view
            --
            print input
