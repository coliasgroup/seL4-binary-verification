module BV.TargetDir
    ( ReadTargetDirFileException (..)
    , TargetDir (..)
    , TargetDirFile (..)
    , TargetDirFiles (..)
    , UntypedTargetDirFile (..)
    , eraseTargetDirFileType
    , readGluedStagesInput
    , readGluedStagesInputEither
    , readTargetDirFile
    , readTargetDirFileEither
    , targetDirFilePath
    , targetDirFiles
    , writeTargetDirFile
    ) where

import BV.ConcreteSyntax
import BV.Core.GluedStages
import BV.Core.Types

import Control.Exception (Exception (..), throwIO)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans (lift)
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
      , proofChecks :: TargetDirFile (FlattenedProofChecks String)
      , smtProofChecks :: TargetDirFile (FlattenedSMTProofChecks ())
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

readGluedStagesInputEither :: (Ident -> Bool) -> TargetDir -> IO (Either ReadTargetDirFileException GluedStagesInput)
readGluedStagesInputEither asmFunctionFilter targetDir = runExceptT $ do
    cFunctions <- f targetDirFiles.cFunctions
    asmFunctions <- f targetDirFiles.asmFunctions
    objDumpInfo <- f targetDirFiles.symtab
    stackBounds <- f targetDirFiles.stackBounds
    inlineScripts <- f targetDirFiles.inlineScripts
    proofs <- f targetDirFiles.proofs
    return $ GluedStagesInput
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
    f file = lift (readTargetDirFileEither targetDir file) >>= liftEither

readGluedStagesInput :: (Ident -> Bool) -> TargetDir -> IO GluedStagesInput
readGluedStagesInput asmFunctionFilter targetDir =
    readGluedStagesInputEither asmFunctionFilter targetDir >>= either throwIO return
