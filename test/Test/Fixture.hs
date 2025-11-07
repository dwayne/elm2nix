module Test.Fixture (file) where

import System.Environment (getEnv)


file :: FilePath -> IO FilePath
file path = do
    projectRoot <- getEnv "PROJECT_ROOT"
    return $ projectRoot ++ "/test/data/" ++ path
