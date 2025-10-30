module Main (main) where

import qualified Elm2Nix.Data.FixedOutputDerivation as FOD


main :: IO ()
main =
    FOD.fromFile "elm.json" >>= either print print
