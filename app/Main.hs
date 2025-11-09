module Main (main) where

import qualified Elm2Nix


main :: IO ()
main =
    Elm2Nix.writeElmLockFile Elm2Nix.Expanded "test/data/elm.json" "elm.lock" >>= either print (const $ putStrLn "Generated elm.lock!")
