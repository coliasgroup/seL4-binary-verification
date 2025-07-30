{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main
    ( main
    ) where

import BV.Core.Prelude
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
    , testCase "stages-with-reference" testStagesWithReferenceDefault
    , testCase "just-focused" $ testStagesWithReference testSeL4TargetDirFocused
    ]

testStages :: IO ()
testStages = do
    input <- seL4DefaultReadStagesInput referenceTargetDir
    let output = stages input
    output.intermediate.compatSMTProofChecks `deepseq` return ()
  where
    referenceTargetDir =
        -- testSeL4TargetDirBig
        testSeL4TargetDirSmall
        -- testSeL4TargetDirFocused

testStagesWithReference :: TargetDir -> IO ()
testStagesWithReference referenceTargetDir = do
    input <- seL4DefaultReadStagesInput referenceTargetDir
    runStderrLoggingT $ evalStages ctx input
    return ()
  where
    ctx = EvalStagesContext
        { forceAll = True
        , forceFingerprints = True
        , dumpTargetDir = Nothing
        , referenceTargetDir = Just referenceTargetDir
        , mismatchDumpDir = Just $ tmpDir </> "mismatch"
        }

testStagesWithReferenceDefault :: IO ()
testStagesWithReferenceDefault = testStagesWithReference
    -- testSeL4TargetDirBig
    testSeL4TargetDirSmall
    -- testSeL4TargetDirFocused
