{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main
    ( main
    ) where

import BV.System.EvalStages
import BV.System.SeL4
import BV.TargetDir
import BV.Test.Utils

import Control.DeepSeq (force, ($!!))
import Control.Monad.Logger (runStderrLoggingT)
import System.FilePath ((</>))

main :: IO ()
main = do
    input <- readStagesInput defaultSeL4AsmFunctionFilter referenceTargetDir
    runStderrLoggingT $ evalStages ctx $!! input
    return ()
  where
    ctx = EvalStagesContext
        { force = True
        , dumpTargetDir = Nothing
        , referenceTargetDir = Just referenceTargetDir
        , mismatchDumpDir = Just $ tmpDir </> "mismatch"
        }
    referenceTargetDir =
        testSeL4TargetDirBig
        -- testSeL4TargetDirSmall
        -- testSeL4TargetDirFocused
