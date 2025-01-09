{-# LANGUAGE AllowAmbiguousTypes #-}

module BV.System.Test.Main
    ( main
    ) where

import Control.DeepSeq (($!!))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import qualified GHC.DataSize
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import BV.ConcreteSyntax (BuildToFile, InBlockAsFile, InLineAsInBlock,
                          ParseFile, buildFile, buildProofChecksForManyFile,
                          parseProofChecksForManyFile, parseWholeFile)
import BV.Core.Types
import BV.System.TargetDir
import BV.System.Test.Utils

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
testReaderSeL4 f = testReader (f testSeL4TargetDir)

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
testRoundTripSeL4 f = testRoundTrip (f testSeL4TargetDir)

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
    , testCase "proof checks" $
        testRoundTripWith
            parseProofChecksForManyFile
            (B.toLazyText . buildProofChecksForManyFile)
            (readProofChecks testSeL4TargetDir)
    -- , testCase "proof checks size" proofChecksSize
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

proofChecksSize :: IO ()
proofChecksSize = do
    r <- readProofChecks testSeL4TargetDir
    case r of
        Left err -> assertFailure err
        Right x -> do
            n <- GHC.DataSize.recursiveSize $!! x
            putStrLn $ "proof checks size: " <> show n

-- ttt :: IO ()
-- ttt = do
--         ttt = tt
--         r = parseWholeFile "" ttt
--     case r of
--         Left err -> do
--             putStrLn err
--             error ""
--         Right ex -> do
--             let x :: InBlockAsFile (InLineAsInBlock ProofNode) = ex
--             -- let x :: InBlockAsFile (InLineAsInBlock Expr) = ex
--             print ex

-- ttt :: IO ()
-- ttt = do
--     r <- readProblems testSeL4TargetDir
--     case r of
--         Left err -> putStrLn err
--         _ -> return ()

-- ttt :: IO ()
-- ttt = do
--     r <- readCFunctions testSeL4TargetDir
--     case r of
--         Left err -> putStrLn err
--         Right x -> do
--             let t = buildFile x
--             L.writeFile (tmpOutPath "snd.txt") t
--             case parseWholeFile "second trip" (L.toStrict t) of
--                 Left err -> putStrLn err
--                 Right x' -> print $ x == x'
