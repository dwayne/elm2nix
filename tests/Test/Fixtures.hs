module Test.Fixtures (fixture) where

import Paths_elm2nix (getDataFileName)


fixture :: FilePath -> IO FilePath
fixture = getDataFileName . (<>) "tests/fixtures/"
