module BV.Test.Main
    ( main
    ) where

import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Test.Tasty
import Test.Tasty.HUnit

import BV.ObjDump
import BV.Parsing (ParseFile, parseWholeFile)
import BV.Printing (BuildToFile, buildFile)
import BV.SeL4
import BV.TargetDir
import BV.Test.Utils
import Control.Monad (when)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "trivial" $ return ()
    , testCase "read objdump info" $ testReader readObjDumpInfo
    , testCase "round trip c functions" $ testRoundTrip readCFunctions
    , testCase "round trip asm functions" $ testRoundTrip readAsmFunctions
    , testCase "round trip functions" $ testRoundTrip readFunctions
    ]

testReader :: (TargetDir -> IO (Either String a)) -> IO ()
testReader f = do
    r <- f testSeL4TargetDir
    case r of
        Left err -> assertFailure (show err)
        _ -> return ()

testRoundTrip :: (Eq a, ParseFile a, BuildToFile a) => (TargetDir -> IO (Either String a)) -> IO ()
testRoundTrip f = do
    r <- f testSeL4TargetDir
    case r of
        Left err -> assertFailure (show err)
        Right x -> do
            let t = buildFile x
            case parseWholeFile "second trip" (L.toStrict t) of
                Left err -> assertFailure (show err)
                Right x' -> assertBool "equal" (x == x')

ttt :: IO ()
ttt = do
    r <- readCFunctions testSeL4TargetDir
    case r of
        Left err -> putStrLn err
        Right x -> do
            let t = buildFile x
            L.writeFile (tmpOutPath "snd.txt") t
            case parseWholeFile "second trip" (L.toStrict t) of
                Left err -> putStrLn err
                Right x' -> print $ x == x'
