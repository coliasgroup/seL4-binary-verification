{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main
    ( main
    ) where

import BV.Test.Utils

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "trivial" $ return ()
    , testCase "inlining" testInlining
    , testCase "stack-bounds" testStackBounds
    ]

testInlining :: IO ()
testInlining = do
    _input <- seL4DefaultReadStagesInput referenceTargetDir
    return ()
  where
    referenceTargetDir =
        -- testSeL4TargetDirBig
        testSeL4TargetDirSmall
        -- testSeL4TargetDirFocused

testStackBounds :: IO ()
testStackBounds = do
    _input <- seL4DefaultReadStagesInput referenceTargetDir
    return ()
  where
    referenceTargetDir =
        -- testSeL4TargetDirBig
        testSeL4TargetDirSmall
        -- testSeL4TargetDirFocused
