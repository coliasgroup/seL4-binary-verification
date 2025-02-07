{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main
    ( main
    ) where

import BV.System.EvalStages
import BV.System.SeL4
import BV.TargetDir
import BV.Test.Utils
import BV.Core.Types
import BV.Core.Stages

import Control.DeepSeq (force, ($!!), deepseq)
import Control.Monad.Logger (runStderrLoggingT)
import System.FilePath ((</>))
import Control.Parallel.Strategies
import qualified Data.Map as M
import BV.ConcreteSyntax
import Crypto.Hash.SHA256
import Data.Text.Lazy.Encoding

main :: IO ()
-- main = main2
main = main1

main1 :: IO ()
main1 = do
    -- input <- force <$> readStagesInput defaultSeL4AsmFunctionFilter referenceTargetDir
    input <- readStagesInput defaultSeL4AsmFunctionFilter referenceTargetDir
    let output = stages input
    -- print $ M.size (force output.x.unwrap)
    -- let x = output.problems
    -- x `deepseq` return ()
    output.compatProofChecks `deepseq` return ()
    -- let txt = writeBVContents output.compatProofChecks
    -- let hash = hashlazy txt
    -- input.objDumpInfo `deepseq` return ()
    -- print hash
    return ()
  where
    ctx = EvalStagesContext
        { force = True
        , dumpTargetDir = Nothing
        -- , referenceTargetDir = Just referenceTargetDir
        , referenceTargetDir = Nothing
        -- , mismatchDumpDir = Just $ tmpDir </> "mismatch"
        , mismatchDumpDir = Nothing
        }
    referenceTargetDir =
        testSeL4TargetDirBig
        -- testSeL4TargetDirSmall
        -- testSeL4TargetDirFocused

main2 :: IO ()
main2 = print (withStrategy strat work)

strat :: Strategy (M.Map Int Int)
strat = traverse rpar

ixs = [1000, 1022, 4328, 1214, 5025, 4021]

work :: M.Map Int Int
work = M.fromList [ (i, fib i) | i <- ixs ]

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
