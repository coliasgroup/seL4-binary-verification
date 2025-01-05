module BV.TargetDir
    ( TargetDir (..)
    , readObjDumpInfo
    ) where

import GHC.Generics (Generic)
import Optics.Core
import System.FilePath ((</>))
import Text.Parsec
import Text.Parsec.Text

import BV.ObjDump
import BV.Parsing
import BV.Program

data TargetDir
  = TargetDir
      { path :: FilePath
      , asmFunctionFilter :: Ident -> Bool
      }
  deriving (Generic)

targetDirPath :: TargetDir -> FilePath -> FilePath
targetDirPath targetDir rel = (targetDir ^. #path) </> rel

readObjDumpInfo :: TargetDir -> IO (Either ParseError ObjDumpInfo)
readObjDumpInfo targetDir = parseFromFile parseObjDumpInfo (targetDirPath targetDir "kernel.elf.symtab")
