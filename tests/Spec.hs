module Main (main) where

import Test.Hspec

import qualified Test.Elm2Nix.Data.Dependency as Dependency
import qualified Test.Elm2Nix.Data.Version as Version
import qualified Test.Elm2Nix.ElmJson as ElmJson


main :: IO ()
main = do
    Version.main
    Dependency.main
    ElmJson.main
