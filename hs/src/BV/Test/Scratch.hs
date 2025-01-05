module BV.Test.Scratch
    (
    ) where

import Data.Maybe

import Text.Pretty.Simple

import BV.TargetDir
import BV.Test.Utils

foo = do
    info <- readObjDumpInfo testSeL4TargetDir
    -- print info
    pPrint info
    writeHtml "bar" info
    putStrLn "success"
