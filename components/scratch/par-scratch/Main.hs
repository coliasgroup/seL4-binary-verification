{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main
    ( main
    ) where

import BV.Core.ExecuteSMTProofChecks
import BV.SMTLIB2.Monad
import BV.System.Backend.Core
import BV.System.Backend.Local
import BV.System.Scratch.Local
import BV.System.SolversConfig
import BV.Test.Utils

import System.FilePath ((</>))

main :: IO ()
main = do
    input <- readStagesInput defaultSeL4AsmFunctionFilter referenceTargetDir
    runStderrLoggingT $ evalStages ctx input
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
