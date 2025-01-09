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
import qualified Data.Text.IO as T

foo = do
    r <- readInterpretedProofChecks "/home/x/i/v/seL4-verification-reproducibility/projects/bv-sandbox/hs/tmp/target-big-interpreted/interpreted-proof-checks.txt"
    case r of
        Left err -> error err
        Right x -> do
            let smt :: [String]
                smt = x ^.. #unwrap % folded % folded % foldExprs % #value % #_ExprValueSmtExpr
            print (length smt)
            writeFile "tmp/out/foo.txt" (mconcat (map (++ "\n") smt))
            -- forM_ smt putStrLn
            putStrLn "success"

readInterpretedProofChecks :: String -> IO (Either String InterpretedProofChecks)
readInterpretedProofChecks path = parseInterpretedProofChecksForManyFile <$> T.readFile path
