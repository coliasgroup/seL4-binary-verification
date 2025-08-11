{-# LANGUAGE AllowAmbiguousTypes #-}

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
main = bvMain $ \opts -> testGroup "Tests"
    [ testGroup "parse/print"
        [ parsePrintGraphRefine opts
        , parsePrintSeL4 opts
        ]
    ]

testReader :: IO (Either String a) -> IO ()
testReader m = do
    r <- m
    case r of
        Left err -> assertFailure (show err)
        _ -> return ()

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

testRoundTripPath :: forall a c. (Eq a, ReadBVFile c a, WriteBVFile c a) => FilePath -> IO ()
testRoundTripPath path = testRoundTrip @a $ readBVFile path

parsePrintSeL4 :: CustomOpts -> TestTree
parsePrintSeL4 opts = testGroup "seL4"
    [ testCase targetDirFiles.symtab.relativePath $ void $ do
        readTargetDirFile opts.defaultTargetDirForFastTests targetDirFiles.symtab
    , testCase targetDirFiles.rodata.relativePath $ do
        objDumpInfo <- readTargetDirFile opts.defaultTargetDirForFastTests targetDirFiles.symtab
        readTargetDirROData objDumpInfo seL4DefaultRODataInputRanges opts.defaultTargetDirForFastTests targetDirFiles.rodata
        return ()
    , f targetDirFiles.cFunctions
    , f targetDirFiles.asmFunctions
    , f targetDirFiles.functions
    , f targetDirFiles.problems
    , f targetDirFiles.stackBounds
    , f targetDirFiles.inlineScripts
    , f targetDirFiles.pairings
    , f targetDirFiles.proofScripts
    , g opts.defaultTargetDirForSlowTests targetDirFiles.proofChecks
    , g opts.defaultTargetDirForSlowTests targetDirFiles.smtProofChecks
    ]
  where
    f :: forall a c. (Eq a, ReadBVFile c a, WriteBVFile c a) => TargetDirFile a -> TestTree
    f = g opts.defaultTargetDirForFastTests
    g :: forall a c. (Eq a, ReadBVFile c a, WriteBVFile c a) => TargetDir -> TargetDirFile a -> TestTree
    g dir file = testCase file.relativePath . testRoundTrip $
        first displayException <$> readTargetDirFileEither dir file

parsePrintGraphRefine :: CustomOpts -> TestTree
parsePrintGraphRefine opts = testGroup "graph-refine" $
    [ f @Program $ "example" </> "Functions.txt"
    , f @Program $ "loop-example" </> "CFuns-annotated.txt"
    , f @Program $ "loop-example" </> "synth" </> "Functions.txt"
    , let rel = "loop-example" </> "O2" </> "proof"
       in testCase rel $ testRoundTripPath @(ProofScript AsmRefineTag ()) (opts.graphRefineDir </> rel)
    ] ++ concatMap g ["O1", "O2"]
  where
    f :: forall a c. (Eq a, ReadBVFile c a, WriteBVFile c a) => FilePath -> TestTree
    f rel = testCase rel $ testRoundTripPath @a (opts.graphRefineDir </> rel)
    g opt =
        [ f @Program $ "loop-example" </> opt </> "ASM-annotated.txt"
        , f @Program $ "loop-example" </> opt </> ("ASM" ++ opt ++ "Funs.txt")
        , f @Program $ "loop-example" </> opt </> "CFunDump.txt"
        , f @StackBounds $ "loop-example" </> opt </> "StackBounds.txt"
        , let rel = "loop-example" </> opt </> ("loop-" ++ opt ++ ".elf.symtab")
           in testCase rel $ testReaderPath @ObjDumpInfo (opts.graphRefineDir </> rel)
        ]
