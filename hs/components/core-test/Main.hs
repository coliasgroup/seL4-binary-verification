{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main
    ( main
    ) where

import Control.Monad.Logger (MonadLogger (monadLoggerLog), runStderrLoggingT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free.Church (FT)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import BV.Core.GluedStages
import BV.System.IntermediateArtifacts
import BV.System.SeL4 (defaultSeL4AsmFunctionFilter)
import BV.TargetDir
import BV.Test.Utils

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "trivial" $ return ()
    , testCase "glued" testGlued
    ]

testGlued :: IO ()
testGlued = do
    input <- either error id <$> readInput defaultSeL4AsmFunctionFilter targetDir
    runStderrLoggingT $ runReaderT
        (runRegisterIntermediateArtifactsT (gluedStages_ input))
        (RegisterIntermediateArtifactsTInnerContext targetDir mismatchDumpDir)
  where
    targetDir = testSeL4TargetDirBig
    mismatchDumpDir = tmpDir </> "mismatch"

instance (MonadLogger m, Functor f) => MonadLogger (FT f m) where
    monadLoggerLog loc source level msg = lift $ monadLoggerLog loc source level msg
