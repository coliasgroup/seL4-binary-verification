module Main
    ( main
    ) where

import BV.Core
import BV.System.EvalStages
import BV.TargetDir
import BV.Test.Utils

import Control.DeepSeq (deepseq)
import Control.Monad.Logger (runStderrLoggingT)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "trivial" $ return ()
    -- , testCase "stages" testStages
    , testCase "stages-with-reference" testStagesWithChecking
    ]

testStages :: IO ()
testStages = do
    input <- readStagesInput seL4DefaultEarlyAsmFunctionFilter referenceTargetDir
    let output = stages input
    output.intermediate.compatProofChecks `deepseq` return ()
  where
    referenceTargetDir =
        testSeL4TargetDirBig
        -- testSeL4TargetDirSmall
        -- testSeL4TargetDirFocused

testStagesWithChecking :: IO ()
testStagesWithChecking = do
    input <- readStagesInput seL4DefaultEarlyAsmFunctionFilter referenceTargetDir
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
        -- testSeL4TargetDirBig
        testSeL4TargetDirSmall
        -- testSeL4TargetDirFocused
