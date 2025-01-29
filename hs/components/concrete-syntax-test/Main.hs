{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main
    ( main
    ) where

import BV.ConcreteSyntax
import BV.Core.Types
import BV.TargetDir
import BV.Test.Utils

import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "trivial" $ return ()
    , testGroup "parse/print"
        [ parsePrintGraphRefine
        , parsePrintSeL4
        ]
    ]

testReader :: IO (Either String a) -> IO ()
testReader m = do
    r <- m
    case r of
        Left err -> assertFailure (show err)
        _ -> return ()

testReaderSeL4 :: (TargetDir -> IO (Either String a)) -> IO ()
testReaderSeL4 f = testReader (f testSeL4TargetDirDefault)

testReaderPath :: forall a c. ReadBVFile c a => FilePath -> IO ()
testReaderPath path = testReader @a (readBVFile path)

testRoundTripWith :: (Eq a, IsContents c) => (c -> Either String a) -> (a -> c) -> IO (Either String a) -> IO ()
testRoundTripWith parse build m = do
    r <- m
    case r of
        Left err -> assertFailure err
        Right x -> do
            let t = build x
            case parse t of
                Left err -> assertFailure err
                Right x' -> assertBool "equal" (x == x')

testRoundTrip :: (Eq a, ReadBVFile c a, WriteBVFile c a) => IO (Either String a) -> IO ()
testRoundTrip = testRoundTripWith (readBVContents "second trip") writeBVContents

testRoundTripSeL4 :: (Eq a, ReadBVFile c a, WriteBVFile c a) => (TargetDir -> IO (Either String a)) -> IO ()
testRoundTripSeL4 f = testRoundTrip (f testSeL4TargetDirDefault)

testRoundTripPath :: forall a c. (Eq a, ReadBVFile c a, WriteBVFile c a) => FilePath -> IO ()
testRoundTripPath path = testRoundTrip @a $ readBVFile path

parsePrintSeL4 :: TestTree
parsePrintSeL4 = testGroup "seL4"
    [ testCase "objdump" $ testReaderSeL4 readObjDumpInfo
    , testCase "c functions" $ testRoundTripSeL4 readCFunctions
    , testCase "asm functions" $ testRoundTripSeL4 readAsmFunctions
    , testCase "functions" $ testRoundTripSeL4 readFunctions
    , testCase "problems" $ testRoundTripSeL4 readProblems
    , testCase "stack bounds" $ testRoundTripSeL4 readStackBounds
    , testCase "inline scripts" $ testRoundTripSeL4 readInlineScripts
    , testCase "pairings" $ testRoundTripSeL4 readPairings
    , testCase "problems and proofs" $ testRoundTripSeL4 readProofs
    , testCase "proof checks" $ testRoundTrip (readProofChecks testSeL4TargetDirSmall)
    -- , testCase "proof checks" $ testRoundTrip (readProofChecks testSeL4TargetDirBig)
    , testCase "smt proof checks" $ testRoundTrip (readSMTProofChecks testSeL4TargetDirSmall)
    -- , testCase "smt proof checks" $ testRoundTrip (readSMTProofChecks testSeL4TargetDirBig)
    ]

parsePrintGraphRefine :: TestTree
parsePrintGraphRefine = testGroup "graph-refine" $
    [ f @Program $ "example" </> "Functions.txt"
    , f @Program $ "loop-example" </> "CFuns-annotated.txt"
    , f @Program $ "loop-example" </> "synth" </> "Functions.txt"
    , let rel = "loop-example" </> "O2" </> "proof"
          abs = graphRefineDir </> rel
       in testCase rel $ testRoundTripPath @(ProofScript ()) (graphRefineDir </> rel)
    ] ++ concatMap g ["O1", "O2"]
  where
    f :: forall a c. (Eq a, ReadBVFile c a, WriteBVFile c a) => FilePath -> TestTree
    f rel = testCase rel $ testRoundTripPath @a (graphRefineDir </> rel)
    g opt =
        [ f @Program $ "loop-example" </> opt </> "ASM-annotated.txt"
        , f @Program $ "loop-example" </> opt </> ("ASM" ++ opt ++ "Funs.txt")
        , f @Program $ "loop-example" </> opt </> "CFunDump.txt"
        , f @StackBounds $ "loop-example" </> opt </> "StackBounds.txt"
        , let rel = "loop-example" </> opt </> ("loop-" ++ opt ++ ".elf.symtab")
           in testCase rel $ testReaderPath @ObjDumpInfo (graphRefineDir </> rel)
        ]
