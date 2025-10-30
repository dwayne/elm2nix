module Main (main) where

import qualified Elm2Nix.ElmLock as ElmLock


main :: IO ()
main =
    ElmLock.generateLockFile "elm.json" "elm.lock" >>= either print (const $ putStrLn "Generated elm.lock!")
