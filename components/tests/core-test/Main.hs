module Main
    ( main
    ) where

import BV.System.EvalStages
import BV.Test.Utils

import Control.Monad.Logger (runStderrLoggingT)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = bvMain $ \opts -> testGroup "Tests"
    [ testCase "stages-with-reference" $ testStagesWithReference opts
    ]

testStagesWithReference :: CustomOpts -> IO ()
testStagesWithReference opts = do
    input <- seL4DefaultReadStagesInput referenceTargetDir
    runStderrLoggingT $ evalStages ctx input
    return ()
  where
    referenceTargetDir = opts.defaultTargetDirForSlowTests
    mismatchDumpDir = mismatchOutDirOf opts
    ctx = EvalStagesContext
        { forceAll = True
        , dumpTargetDir = Nothing
        , referenceTargetDir = Just referenceTargetDir
        , mismatchDumpDir = Just mismatchDumpDir
        }
