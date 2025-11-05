module Main (main) where

import qualified Test.Elm2Nix.Data.Dependency as Dependency
import qualified Test.Elm2Nix.Data.ElmJson as ElmJson
import qualified Test.Elm2Nix.Data.Name as Name
import qualified Test.Elm2Nix.Data.Version as Version


main :: IO ()
main = do
    Name.main
    Version.main
    Dependency.main
    ElmJson.main
