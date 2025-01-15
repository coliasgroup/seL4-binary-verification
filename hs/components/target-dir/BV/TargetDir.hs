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
    , readProblemsAndProofs
    , readProofChecks
    , readSMTProofChecks
    , readStackBounds
    ) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import System.FilePath ((</>))

import BV.ConcreteSyntax
import BV.Core.GluedStages
import BV.Core.Types
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Trans (lift)

-- TODO abstract targetDirEntry field, use MonadError String

data TargetDir
  = TargetDir
      { path :: FilePath
      }
  deriving (Eq, Generic, Ord, Show)

targetDirPath :: TargetDir -> FilePath -> FilePath
targetDirPath targetDir rel = targetDir.path </> rel

readAndParseFile :: ParseFile a => FilePath -> TargetDir -> IO (Either String a)
readAndParseFile rel targetDir = parseWholeFile path <$> T.readFile path
  where
    path = targetDirPath targetDir rel

readAndParseFileFast :: ParseFileFast a => FilePath -> TargetDir -> IO (Either String a)
readAndParseFileFast rel targetDir = parseWholeFileFast <$> T.readFile path
  where
    path = targetDirPath targetDir rel

readInput :: (Ident -> Bool) -> TargetDir -> IO (Either String Input)
readInput asmFunctionFilter targetDir = runExceptT $ do
    cFunctions <- lift (readCFunctions targetDir) >>= liftEither
    asmFunctions <- lift (readAsmFunctions targetDir) >>= liftEither
    objDumpInfo <- lift (readObjDumpInfo targetDir) >>= liftEither
    stackBounds <- lift (readStackBounds targetDir) >>= liftEither
    inlineScripts <- lift (readInlineScripts targetDir) >>= liftEither
    problemsAndProofs <- lift (readProblemsAndProofs targetDir) >>= liftEither
    return $ Input
      { programs = PairingOf
            { c = cFunctions
            , asm = asmFunctions
            }
      , objDumpInfo
      , stackBounds
      , inlineScripts
      , problemsAndProofs
      , asmFunctionFilter
      }

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

readInlineScripts :: TargetDir -> IO (Either String InlineScripts)
readInlineScripts = readAndParseFile "inline-scripts.txt"

readPairings :: TargetDir -> IO (Either String Pairings)
readPairings = readAndParseFile "pairings.txt"

readProblemsAndProofs :: TargetDir -> IO (Either String ProblemsAndProofs)
readProblemsAndProofs = readAndParseFile "proofs.txt"

readProofChecks :: TargetDir -> IO (Either String (ProofChecks String))
readProofChecks = readAndParseFileFast "proof-checks.txt"

readSMTProofChecks :: TargetDir -> IO (Either String (SMTProofChecks ()))
readSMTProofChecks = readAndParseFileFast "smt-proof-checks.txt"
-- readSMTProofChecks = readAndParseFile "smt-proof-checks.txt"
