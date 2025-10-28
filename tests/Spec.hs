module Main (main) where

import Test.Hspec

import qualified Test.Elm2Nix.Version as Version


main :: IO ()
main =
    Version.main
