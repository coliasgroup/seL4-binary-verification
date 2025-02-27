module Main
    ( main
    ) where

import BV.Core
import BV.ConcreteSyntax
import BV.System.EvalStages
import BV.TargetDir
import BV.Test.Utils
import BV.System.Utils.Stopwatch

import Control.DeepSeq (deepseq)
import Data.Functor (void)
import Control.Monad.Logger (runStderrLoggingT)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Codec.Compression.Zlib (compress)

main :: IO ()
main = do
    -- testInput
    testStages
    -- testStagesWithChecking

testInput :: IO ()
testInput = do
    input <- readStagesInput seL4DefaultEarlyAsmFunctionFilter targetDir
    let enc = encode input
    print (B.length enc)
    print (B.length (compress enc))
  where
    targetDir =
        testSeL4TargetDirBig
        -- testSeL4TargetDirSmall
        -- testSeL4TargetDirFocused

testStages :: IO ()
testStages = do
    input <- readStagesInput seL4DefaultEarlyAsmFunctionFilter targetDir
    let output = stages input
    -- output.intermediate.compatProofChecks `deepseq` return ()
    -- void output.intermediate.proofChecks `deepseq` return ()
    let o = output.intermediate.compatProofChecks
    wt o
    putStrLn "write"
    let c = writeBVContents o
    wt c
    putStrLn "read"
    let r = readBVContents "" c
    wt r
    putStrLn "eq"
    let e = r == Right o
    wt e
    print e
  where
    targetDir =
        testSeL4TargetDirBig
        -- testSeL4TargetDirSmall
        -- testSeL4TargetDirFocused
    wt v = do
        (_, t) <- time $ v `deepseq` return ()
        putStrLn $ show (fromRational (elapsedToSeconds t) :: Double) ++ "s"

testStagesWithChecking :: IO ()
testStagesWithChecking = do
    input <- readStagesInput seL4DefaultEarlyAsmFunctionFilter referenceTargetDir
    runStderrLoggingT $ evalStages ctx input
    return ()
  where
    ctx = EvalStagesContext
        { force = True
        , dumpTargetDir = Nothing
        , referenceTargetDir = Just referenceTargetDir
        , mismatchDumpDir = Just $ tmpDir </> "mismatch"
        }
    referenceTargetDir =
        testSeL4TargetDirBig
        -- testSeL4TargetDirSmall
        -- testSeL4TargetDirFocused
