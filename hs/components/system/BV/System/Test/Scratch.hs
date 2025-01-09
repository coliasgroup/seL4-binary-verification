module BV.System.Test.Scratch
    (
    ) where

import Text.Pretty.Simple

import BV.System.TargetDir
import BV.System.Test.Utils

foo = do
    info <- readObjDumpInfo testSeL4TargetDirBig
    -- print info
    pPrint info
    writeHtml "bar" info
    putStrLn "success"
