module BV.TargetDir
    ( TargetDir (..)
    , readAsmFunctions
    , readCFunctions
    , readFunctions
    , readObjDumpInfo
    , readProblems
    ) where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Optics.Core
import System.FilePath ((</>))
import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse)

import BV.ObjDump
import BV.Pairing
import BV.Parsing
import BV.Problem
import BV.Program

data TargetDir
  = TargetDir
      { path :: FilePath
      , asmFunctionFilter :: Ident -> Bool
      }
  deriving (Generic)

targetDirPath :: TargetDir -> FilePath -> FilePath
targetDirPath targetDir rel = targetDir.path </> rel

readAndParseFile :: ParseFile a => FilePath -> TargetDir -> IO (Either String a)
readAndParseFile rel targetDir = parseWholeFile path <$> T.readFile path
  where
    path = targetDirPath targetDir rel

readObjDumpInfo :: TargetDir -> IO (Either String ObjDumpInfo)
readObjDumpInfo = readAndParseFile "kernel.elf.symtab"

readCFunctions :: TargetDir -> IO (Either String Program)
readCFunctions = readAndParseFile "CFunctions.txt"

readAsmFunctions :: TargetDir -> IO (Either String Program)
readAsmFunctions = readAndParseFile "ASMFunctions.txt"

readFunctions :: TargetDir -> IO (Either String Program)
readFunctions = readAndParseFile "functions.txt"

readProblems :: TargetDir -> IO (Either String Problems)
readProblems = readAndParseFile "problems.txt"
