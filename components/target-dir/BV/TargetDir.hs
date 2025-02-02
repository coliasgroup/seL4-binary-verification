module BV.TargetDir
    ( ReadTargetDirFileException (..)
    , TargetDir (..)
    , TargetDirFile (..)
    , TargetDirFiles (..)
    , UntypedTargetDirFile (..)
    , eraseTargetDirFileType
    , readStagesInput
    , readStagesInputEither
    , readTargetDirFile
    , readTargetDirFileEither
    , targetDirFilePath
    , targetDirFiles
    , writeTargetDirFile
    ) where

import BV.ConcreteSyntax
import BV.Core.Stages
import BV.Core.Types

import Control.Exception (Exception (..), throwIO)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Bifunctor (first)
import GHC.Generics (Generic)
import System.FilePath ((</>))

newtype TargetDir
  = TargetDir { path :: FilePath }
  deriving (Eq, Generic, Ord, Show)

newtype TargetDirFile a
  = TargetDirFile { relativePath :: FilePath }
  deriving (Eq, Generic, Ord, Show)

newtype UntypedTargetDirFile
  = UntypedTargetDirFile { relativePath :: FilePath }
  deriving (Eq, Generic, Ord, Show)

eraseTargetDirFileType :: TargetDirFile a -> UntypedTargetDirFile
eraseTargetDirFileType (TargetDirFile relativePath) = UntypedTargetDirFile relativePath

data TargetDirFiles
  = TargetDirFiles
      { symtab :: TargetDirFile ObjDumpInfo
      , cFunctions :: TargetDirFile Program
      , asmFunctions :: TargetDirFile Program
      , functions :: TargetDirFile Program
      , problems :: TargetDirFile Problems
      , stackBounds :: TargetDirFile StackBounds
      , inlineScripts :: TargetDirFile InlineScripts
      , pairings :: TargetDirFile Pairings
      , proofs :: TargetDirFile (Proofs ())
      , proofChecks :: TargetDirFile (CompatProofChecks String)
      , smtProofChecks :: TargetDirFile (CompatSMTProofChecks ())
      }
  deriving (Eq, Generic, Ord, Show)

targetDirFiles :: TargetDirFiles
targetDirFiles = TargetDirFiles
    { symtab = TargetDirFile "kernel.elf.symtab"
    , cFunctions = TargetDirFile "CFunctions.txt"
    , asmFunctions = TargetDirFile "ASMFunctions.txt"
    , functions = TargetDirFile "functions.txt"
    , problems = TargetDirFile "problems.txt"
    , stackBounds = TargetDirFile "StackBounds.txt"
    , inlineScripts = TargetDirFile "inline-scripts.json"
    , pairings = TargetDirFile "pairings.json"
    , proofs = TargetDirFile "proofs.json"
    , proofChecks = TargetDirFile "proof-checks.json"
    , smtProofChecks = TargetDirFile "smt-proof-checks.json"
    }

targetDirFilePath :: TargetDir -> UntypedTargetDirFile -> FilePath
targetDirFilePath targetDir targetDirFile = targetDir.path </> targetDirFile.relativePath

readTargetDirFileEither :: ReadBVFile c a => TargetDir -> TargetDirFile a -> IO (Either ReadTargetDirFileException a)
readTargetDirFileEither targetDir targetDirFile =
    first elaborate <$> readBVFile (targetDirFilePath targetDir untypedTargetDirFile)
  where
    untypedTargetDirFile = eraseTargetDirFileType targetDirFile
    elaborate message =
        ReadTargetDirFileException
            { message
            , targetDir
            , targetDirFile = untypedTargetDirFile
            }

readTargetDirFile :: ReadBVFile c a => TargetDir -> TargetDirFile a -> IO a
readTargetDirFile targetDir targetDirFile =
    readTargetDirFileEither targetDir targetDirFile >>= either throwIO return

data ReadTargetDirFileException
  = ReadTargetDirFileException
      { targetDir :: TargetDir
      , targetDirFile :: UntypedTargetDirFile
      , message :: String
      }
  deriving (Eq, Generic, Ord, Show)

instance Exception ReadTargetDirFileException where
    displayException e =
        "ReadTargetDirFileException at "
        ++ targetDirFilePath e.targetDir e.targetDirFile
        ++ ":\n"
        ++ e.message

writeTargetDirFile :: WriteBVFile c a => TargetDir -> TargetDirFile a -> a -> IO ()
writeTargetDirFile targetDir targetDirFile =
    writeBVFile (targetDirFilePath targetDir (eraseTargetDirFileType targetDirFile))

readStagesInputEither :: (Ident -> Bool) -> TargetDir -> IO (Either ReadTargetDirFileException StagesInput)
readStagesInputEither asmFunctionFilter targetDir = runExceptT $ do
    cFunctions <- f targetDirFiles.cFunctions
    asmFunctions <- f targetDirFiles.asmFunctions
    objDumpInfo <- f targetDirFiles.symtab
    stackBounds <- f targetDirFiles.stackBounds
    inlineScripts <- f targetDirFiles.inlineScripts
    proofs <- f targetDirFiles.proofs
    return $ StagesInput
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
  where
    f file = ExceptT $ readTargetDirFileEither targetDir file

readStagesInput :: (Ident -> Bool) -> TargetDir -> IO StagesInput
readStagesInput asmFunctionFilter targetDir =
    readStagesInputEither asmFunctionFilter targetDir >>= either throwIO return
