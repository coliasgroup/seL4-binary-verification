module Main
    ( main
    ) where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as B
import System.Environment

import BV.ConcreteSyntax
import BV.Core.Types
import BV.System.TargetDir
import BV.System.Test.Utils

main :: IO ()
main = do
    path:_ <- getArgs
    r <- readProofChecks (TargetDir path)
    case r of
        Left err -> error err
        Right x -> do
            let t = (B.toLazyText . buildProofChecksForManyFile) x
            print t
            print $ T.length t `div` 1024
