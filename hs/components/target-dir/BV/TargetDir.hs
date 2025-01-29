module BV.TargetDir
    ( TargetDir (..)
    , readAsmFunctions
    , readCFunctions
    , readFunctions
    , readInlineScripts
    , readInput
    , readObjDumpInfo
    , readPairings
    , readProblems
    , readProofChecks
    , readProofs
    , readSMTProofChecks
    , readStackBounds
    ) where

import BV.ConcreteSyntax
import BV.Core.GluedStages
import BV.Core.Types

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans (lift)
import GHC.Generics (Generic)
import System.FilePath ((</>))

data TargetDir
  = TargetDir
      { path :: FilePath
      }
  deriving (Eq, Generic, Ord, Show)

targetDirPath :: TargetDir -> FilePath -> FilePath
targetDirPath targetDir rel = targetDir.path </> rel

readTargetDirFile :: ReadBVFile c a => FilePath -> TargetDir -> IO (Either String a)
readTargetDirFile rel targetDir = readBVFile (targetDirPath targetDir rel)

readInput :: (Ident -> Bool) -> TargetDir -> IO (Either String Input)
readInput asmFunctionFilter targetDir = runExceptT $ do
    cFunctions <- lift (readCFunctions targetDir) >>= liftEither
    asmFunctions <- lift (readAsmFunctions targetDir) >>= liftEither
    objDumpInfo <- lift (readObjDumpInfo targetDir) >>= liftEither
    stackBounds <- lift (readStackBounds targetDir) >>= liftEither
    inlineScripts <- lift (readInlineScripts targetDir) >>= liftEither
    proofs <- lift (readProofs targetDir) >>= liftEither
    return $ Input
      { programs = PairingOf
            { c = cFunctions
            , asm = asmFunctions
            }
      , objDumpInfo
      , stackBounds
      , inlineScripts
      , proofs
      , asmFunctionFilter
      }

readObjDumpInfo :: TargetDir -> IO (Either String ObjDumpInfo)
readObjDumpInfo = readTargetDirFile "kernel.elf.symtab"

readCFunctions :: TargetDir -> IO (Either String Program)
readCFunctions = readTargetDirFile "CFunctions.txt"

readAsmFunctions :: TargetDir -> IO (Either String Program)
readAsmFunctions = readTargetDirFile "ASMFunctions.txt"

readFunctions :: TargetDir -> IO (Either String Program)
readFunctions = readTargetDirFile "functions.txt"

readProblems :: TargetDir -> IO (Either String Problems)
readProblems = readTargetDirFile "problems.txt"

readStackBounds :: TargetDir -> IO (Either String StackBounds)
readStackBounds = readTargetDirFile "StackBounds.txt"

readInlineScripts :: TargetDir -> IO (Either String InlineScripts)
readInlineScripts = readTargetDirFile "inline-scripts.txt"

readPairings :: TargetDir -> IO (Either String Pairings)
readPairings = readTargetDirFile "pairings.txt"

readProofs :: TargetDir -> IO (Either String (Proofs ()))
readProofs = readTargetDirFile "proofs.txt"

readProofChecks :: TargetDir -> IO (Either String (FlattenedProofChecks String))
readProofChecks = readTargetDirFile "proof-checks.json"

readSMTProofChecks :: TargetDir -> IO (Either String (FlattenedSMTProofChecks ()))
readSMTProofChecks = readTargetDirFile "smt-proof-checks.json"
