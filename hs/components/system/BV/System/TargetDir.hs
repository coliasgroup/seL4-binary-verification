module BV.System.TargetDir
    ( TargetDir (..)
    , readAsmFunctions
    , readCFunctions
    , readFunctions
    , readObjDumpInfo
    , readPairings
    , readProblems
    , readProblemsAndProofs
    , readProofChecks
    , readStackBounds
    ) where

import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import System.FilePath ((</>))

import BV.ConcreteSyntax
import BV.Core.Types

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

readStackBounds :: TargetDir -> IO (Either String StackBounds)
readStackBounds = readAndParseFile "StackBounds.txt"

readPairings :: TargetDir -> IO (Either String Pairings)
readPairings = readAndParseFile "pairings.txt"

readProblemsAndProofs :: TargetDir -> IO (Either String ProblemsAndProofs)
readProblemsAndProofs = readAndParseFile "proofs.txt"

readProofChecks :: TargetDir -> IO (Either String (ProofChecks String))
readProofChecks targetDir = parseProofChecksForManyFile <$> T.readFile path
  where
    path = targetDirPath targetDir "proof-checks.txt"
