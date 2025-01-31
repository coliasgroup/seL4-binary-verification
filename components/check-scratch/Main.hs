module Main
    ( main
    ) where

import BV.System.LocalCheckBackend.Scratch (runScratch)
import BV.Test.Utils

import System.FilePath ((</>))

main :: IO ()
main = runScratch
    testSeL4TargetDirSmall
    (tmpDir </> "mismatch")
