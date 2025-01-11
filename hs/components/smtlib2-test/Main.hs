module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import BV.SMTLIB2

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "trivial" $ return ()
    ]
