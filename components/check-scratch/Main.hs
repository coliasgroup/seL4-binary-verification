module Main
    ( main
    ) where

import BV.System.CheckScratch (runScratch)
import BV.Test.Utils

main :: IO ()
main = runScratch
    testSeL4TargetDirSmall
