module Main (main) where

import qualified Test.Elm2Nix.Data.Dependency as Dependency
import qualified Test.Elm2Nix.Data.ElmJson as ElmJson
import qualified Test.Elm2Nix.Data.ElmLock as ElmLock
import qualified Test.Elm2Nix.Data.FixedOutputDerivation as FixedOutputDerivation
import qualified Test.Elm2Nix.Data.Name as Name
import qualified Test.Elm2Nix.Data.RegistryDat as RegistryDat
import qualified Test.Elm2Nix.Data.Version as Version
import qualified Test.Elm2Nix.Lib.Binary as Binary
import qualified Test.Elm2Nix.Lib.Json.Decode as JD
import qualified Test.Elm2Nix.Lib.Nix as Nix


main :: IO ()
main = do
    Name.main
    Version.main
    Dependency.main
    ElmJson.main
    ElmLock.main
    Nix.main
    JD.main
    FixedOutputDerivation.main
    RegistryDat.main
    Binary.main
