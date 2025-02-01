{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main
    ( main
    ) where

import BV.System.EvalStages
import BV.System.SeL4 (defaultSeL4AsmFunctionFilter)
import BV.TargetDir
import BV.Test.Utils

import Control.Monad.Logger (MonadLogger (monadLoggerLog), runStderrLoggingT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free.Church (FT)
import Data.Functor (void)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "trivial" $ return ()
    , testCase "glued" testGlued
    ]

testGlued :: IO ()
testGlued = do
    input <- readStagesInput defaultSeL4AsmFunctionFilter referenceTargetDir
    let ctx = EvalStagesContext
            { force = True
            , dumpTargetDir = Nothing
            , referenceTargetDir = Just referenceTargetDir
            , mismatchDumpDir = Just $ tmpDir </> "mismatch"
            }
    void . runStderrLoggingT $ evalStages ctx input
  where
    -- referenceTargetDir = testSeL4TargetDirBig
    referenceTargetDir = testSeL4TargetDirSmall
    -- referenceTargetDir = testSeL4TargetDirFocused

instance (MonadLogger m, Functor f) => MonadLogger (FT f m) where
    monadLoggerLog loc source level msg = lift $ monadLoggerLog loc source level msg
