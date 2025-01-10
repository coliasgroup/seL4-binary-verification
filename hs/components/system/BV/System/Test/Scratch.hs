{-# LANGUAGE OverloadedStrings #-}

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
import Data.Attoparsec.Text (parseOnly)
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TL

foo = do
    -- r <- readSmtProofChecks testSeL4TargetDirBigSmt
    -- x <- B.readFile "/home/x/i/v/seL4-verification-reproducibility/projects/bv-sandbox/hs/tmp/target-small-smt/smt-proof-checks.json"
    -- let x = "()"
    -- let x = "fsd"
    -- let r = parseOnly parseSExpr x
    -- print r
    -- error "done"
    -- x <- T.readFile "/home/x/i/v/seL4-verification-reproducibility/projects/bv-sandbox/hs/tmp/out/x.txt"
    x <- T.readFile "/home/x/i/v/seL4-verification-reproducibility/projects/bv-sandbox/hs/tmp/target-small-smt/smt-proof-checks.txt"
    let r = parseWholeFileFast @(SmtProofChecks) x
    case r of
        Left err -> error err
        Right x -> do
            -- let smt :: [String]
            --     smt = x ^.. #unwrap % folded % folded % #setup % folded
            -- print (length smt)
            -- print (M.size x.unwrap)
            TL.writeFile "tmp/out/foo.txt" (TB.toLazyText (buildToFile x))
            -- forM_ smt putStrLn
            putStrLn "success"
