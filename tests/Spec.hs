module Main (main) where

import Test.Hspec

import qualified Test.Elm2Nix.Dependency as Dependency
import qualified Test.Elm2Nix.Version as Version


main :: IO ()
main = do
    Dependency.main
    Version.main
