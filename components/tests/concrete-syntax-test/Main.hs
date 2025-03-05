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

import Control.Exception (Exception (displayException))
import Data.Bifunctor (first)
import Data.Functor (void)
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

testReaderSeL4 :: ReadBVFile c a => TargetDirFile a -> IO ()
testReaderSeL4 = void . readTargetDirFile testSeL4TargetDirDefault

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

testRoundTripSeL4 :: (Eq a, ReadBVFile c a, WriteBVFile c a) => TargetDirFile a -> IO ()
testRoundTripSeL4 targetDirFile =
    testRoundTrip (first displayException <$> readTargetDirFileEither testSeL4TargetDirDefault targetDirFile)

testRoundTripPath :: forall a c. (Eq a, ReadBVFile c a, WriteBVFile c a) => FilePath -> IO ()
testRoundTripPath path = testRoundTrip @a $ readBVFile path

parsePrintSeL4 :: TestTree
parsePrintSeL4 = testGroup "seL4"
    [ f targetDirFiles.symtab
    , testCase targetDirFiles.rodata.relativePath $ do
        objDumpInfo <- readTargetDirFile testSeL4TargetDirDefault targetDirFiles.symtab
        readTargetDirROData objDumpInfo seL4DefaultRODataInputRanges testSeL4TargetDirDefault targetDirFiles.rodata
        return ()
    , f targetDirFiles.cFunctions
    , f targetDirFiles.asmFunctions
    , f targetDirFiles.functions
    , f targetDirFiles.problems
    , f targetDirFiles.stackBounds
    , f targetDirFiles.inlineScripts
    , f targetDirFiles.pairings
    , f targetDirFiles.proofs
    , g testSeL4TargetDirSmall targetDirFiles.proofChecks
    -- , g testSeL4TargetDirBig targetDirFiles.proofChecks
    , g testSeL4TargetDirSmall targetDirFiles.smtProofChecks
    -- , g testSeL4TargetDirBig targetDirFiles.smtProofChecks
    ]
  where
    f file = testCase file.relativePath $ testReaderSeL4 file
    g dir file = testCase file.relativePath . testRoundTrip $
        first displayException <$> readTargetDirFileEither dir file

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
