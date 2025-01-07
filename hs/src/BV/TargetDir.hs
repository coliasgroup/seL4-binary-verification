module BV.TargetDir
    ( TargetDir (..)
    , readObjDumpInfo
    ) where

import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Optics.Core
import System.FilePath ((</>))
import Text.Megaparsec (eof, errorBundlePretty, parse)

import BV.ObjDump
import BV.Program
import Data.Bifunctor (first)

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
    s <- T.readFile path
    return . first errorBundlePretty $ parse (parseObjDumpInfo <* eof) path s
  where
    path = targetDirPath targetDir "kernel.elf.symtab"
