module Main (main) where

import Test.Hspec

import qualified Test.Elm2Nix.Data.Dependency as Dependency
import qualified Test.Elm2Nix.Data.Name as Name
import qualified Test.Elm2Nix.Data.Version as Version
import qualified Test.Elm2Nix.ElmJson as ElmJson


main :: IO ()
main = do
    Name.main
    Version.main
    Dependency.main
    ElmJson.main
