module BV.System.Test.Scratch
    (
    ) where

import Optics.Core
import Text.Pretty.Simple

import BV.ConcreteSyntax
import BV.Core.Types
import BV.System.SeL4
import BV.System.TargetDir
import BV.System.Test.Utils
import Control.Monad
import Data.Attoparsec (parseOnly)
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text.IO as T

foo = do
    -- r <- readSmtProofChecks testSeL4TargetDirBigSmt
    -- x <- B.readFile "/home/x/i/v/seL4-verification-reproducibility/projects/bv-sandbox/hs/tmp/target-small-smt/smt-proof-checks.json"
    x <- B.readFile "/home/x/i/v/seL4-verification-reproducibility/projects/bv-sandbox/hs/tmp/target-small/proof-checks.txt"
    let r = parseWholeFileFast @(ProofChecks String) x
    case r of
        Left err -> error err
        Right x -> do
            -- let smt :: [String]
            --     smt = x ^.. #unwrap % folded % folded % #setup % folded
            -- print (length smt)
            print (M.size x.unwrap)
            -- writeFile "tmp/out/foo.txt" (mconcat (map (++ "\n") smt))
            -- forM_ smt putStrLn
            putStrLn "success"
