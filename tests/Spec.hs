module Main (main) where

import Test.Hspec


main :: IO ()
main = hspec $
    describe "Example" $
        it "1 == 1" $
            1 == 1
