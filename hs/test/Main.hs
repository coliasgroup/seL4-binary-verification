module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import BV.ObjDump
import BV.SeL4
import BV.TargetDir
import BV.Test.Utils

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "trivial" (return ())
    , testObjDumpInfo
    ]

testObjDumpInfo :: TestTree
testObjDumpInfo = testCase "objdump info" $ do
    info <- readObjDumpInfo testSeL4TargetDir
    case info of
        Left err -> assertFailure (show err)
        _ -> return ()
