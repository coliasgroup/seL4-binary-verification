module BV.TargetDir
    ( TargetDir (..)
    , readObjDumpInfo
    ) where

import Data.Attoparsec.Text (endOfInput, parseOnly)
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Optics.Core
import System.FilePath ((</>))

import BV.ObjDump
import BV.Program

data TargetDir
  = TargetDir
      { path :: FilePath
      , asmFunctionFilter :: Ident -> Bool
      }
  deriving (Generic)

targetDirPath :: TargetDir -> FilePath -> FilePath
targetDirPath targetDir rel = (targetDir ^. #path) </> rel

readObjDumpInfo :: TargetDir -> IO (Either String ObjDumpInfo)
readObjDumpInfo targetDir = do
    s <- T.readFile $ targetDirPath targetDir "kernel.elf.symtab"
    return $ parseOnly (parseObjDumpInfo <* endOfInput) s
