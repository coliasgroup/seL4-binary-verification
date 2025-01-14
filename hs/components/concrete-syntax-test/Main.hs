{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main
    ( main
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import BV.ConcreteSyntax
import BV.ConcreteSyntax.TargetDir
import BV.Core.Types
import BV.Test.Utils

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

testReaderPath :: forall a. ParseFile a => FilePath -> IO ()
testReaderPath path = (testReader @a) $ parseWholeFile path <$> T.readFile path

testRoundTripWith :: Eq a => (T.Text -> Either String a) -> (a -> L.Text) -> IO (Either String a) -> IO ()
testRoundTripWith parse build m = do
    r <- m
    case r of
        Left err -> assertFailure err
        Right x -> do
            let t = build x
            case parse (L.toStrict t) of
                Left err -> assertFailure err
                Right x' -> assertBool "equal" (x == x')

testRoundTrip :: (Eq a, ParseFile a, BuildToFile a) => IO (Either String a) -> IO ()
testRoundTrip = testRoundTripWith (parseWholeFile "second trip") buildFile

testRoundTripSeL4 :: (Eq a, ParseFile a, BuildToFile a) => (TargetDir -> IO (Either String a)) -> IO ()
testRoundTripSeL4 f = testRoundTrip (f testSeL4TargetDirDefault)

testRoundTripPath :: forall a. (Eq a, ParseFile a, BuildToFile a) => FilePath -> IO ()
testRoundTripPath path = (testRoundTrip @a) $ parseWholeFile path <$> T.readFile path

parsePrintSeL4 :: TestTree
parsePrintSeL4 = testGroup "seL4"
    [ testCase "objdump" $ testReaderSeL4 readObjDumpInfo
    , testCase "c functions" $ testRoundTripSeL4 readCFunctions
    , testCase "asm functions" $ testRoundTripSeL4 readAsmFunctions
    , testCase "functions" $ testRoundTripSeL4 readFunctions
    , testCase "problems" $ testRoundTripSeL4 readProblems
    , testCase "stack bounds" $ testRoundTripSeL4 readStackBounds
    , testCase "pairings" $ testRoundTripSeL4 readPairings
    , testCase "problems and proofs" $ testRoundTripSeL4 readProblemsAndProofs
    , testCase "smt proof checks" $
        testRoundTripWith
            -- (parseWholeFile "")
            parseWholeFileFast
            buildFile
            (readSmtProofChecks testSeL4TargetDirSmallSmt)
            -- (readSmtProofChecks testSeL4TargetDirBigSmt)
    -- , testCase "proof checks" $
    --     testRoundTripWith
    --         parseWholeFileFast
    --         buildFile
    --         (readProofChecks testSeL4TargetDirSmall)
    ]

parsePrintGraphRefine :: TestTree
parsePrintGraphRefine = testGroup "graph-refine" $
    [ f @Program $ "example" </> "Functions.txt"
    , f @Program $ "loop-example" </> "CFuns-annotated.txt"
    , f @Program $ "loop-example" </> "synth" </> "Functions.txt"
    , let rel = "loop-example" </> "O2" </> "proof"
          abs = graphRefineDir </> rel
       in testCase rel $ testRoundTripPath @(InBlockAsFile (InLineAsInBlock ProofNode)) (graphRefineDir </> rel)
    ] ++ concatMap g ["O1", "O2"]
  where
    f :: forall a. (Eq a, ParseFile a, BuildToFile a) => FilePath -> TestTree
    f rel = testCase rel $ testRoundTripPath @a (graphRefineDir </> rel)
    g opt =
        [ f @Program $ "loop-example" </> opt </> "ASM-annotated.txt"
        , f @Program $ "loop-example" </> opt </> ("ASM" ++ opt ++ "Funs.txt")
        , f @Program $ "loop-example" </> opt </> "CFunDump.txt"
        , f @StackBounds $ "loop-example" </> opt </> "StackBounds.txt"
        , let rel = "loop-example" </> opt </> ("loop-" ++ opt ++ ".elf.symtab")
           in testCase rel $ testReaderPath @ObjDumpInfo (graphRefineDir </> rel)
        ]
