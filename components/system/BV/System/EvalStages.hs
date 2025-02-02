{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BV.System.EvalStages
    ( EvalStagesContext (..)
    , evalStages
    ) where

import BV.ConcreteSyntax
import BV.Core.Stages
import BV.Core.Types
import BV.TargetDir

import Control.DeepSeq (NFData, deepseq, force)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Data.Function (applyWhen)
import qualified Data.Map as M
import Data.String (fromString)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Optics.Core
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

data EvalStagesContext
  = EvalStagesContext
      { force :: Bool
      , dumpTargetDir :: Maybe TargetDir
      , referenceTargetDir :: Maybe TargetDir
      , mismatchDumpDir :: Maybe FilePath
      }
  deriving (Eq, Generic, Ord, Show)

evalStages
    :: forall m. (MonadIO m, MonadFail m, MonadLogger m)
    => EvalStagesContext
    -> StagesInput
    -> m (SMTProofChecks String)
evalStages ctx input = do
    logWarnN . fromString $
        "Unhandled inline assembly functions (C side): " ++ show (map (.unwrap) output.unhandledInlineAssemblyFunctions)
    logWarnN . fromString $
        "Unhandled instrcution functions (Asm side): " ++ show (map (.unwrap) output.unhandledInstructionFunctions)
    logInfoN "Registering functions"
    register noop targetDirFiles.functions output.functions
    logInfoN "Registering pairings"
    register noop targetDirFiles.pairings output.pairings
    logInfoN "Registering problems"
    register filterProblems targetDirFiles.problems output.problems
    logInfoN "Registering proof checks"
    register noop targetDirFiles.proofChecks output.compatProofChecks
    logInfoN "Registering SMT proof checks"
    register noop targetDirFiles.smtProofChecks output.compatSMTProofChecks
    logInfoN "Registered all intermediate artifacts"
    return output.smtProofChecks
  where
    output = stages input
    register :: forall a c. (Eq a, NFData a, ReadBVFile c a, WriteBVFile c a) => (a -> a -> a) -> TargetDirFile a -> a -> m ()
    register f file actual = applyWhen ctx.force (deepseq actual) $ do
        whenJust ctx.dumpTargetDir $ \dumpTargetDir -> do
            logInfoN $ "Dumping " <> T.pack file.relativePath
            liftIO $ writeTargetDirFile dumpTargetDir file actual
        whenJust ctx.referenceTargetDir $ \referenceTargetDir -> do
            expected <- liftIO $ readTargetDirFile referenceTargetDir file
            let actual' = f expected actual
            when (maybeForce actual' /= maybeForce expected) $ do
                logErrorN $ "Intermediate artifact mismatch for " <> T.pack file.relativePath
                whenJust ctx.mismatchDumpDir $ \mismatchDumpDir -> do
                    let d = mismatchDumpDir </> file.relativePath
                    logInfoN $ "Writing mismatch to " <> T.pack d
                    liftIO $ do
                        createDirectoryIfMissing True d
                        writeBVFile (d </> "actual.txt") actual'
                        writeBVFile (d </> "expected.txt") expected
                fail "Intermediate artifact mismatch"
    maybeForce :: forall a. NFData a => a -> a
    maybeForce = applyWhen ctx.force force
    filterProblems expected actual = actual & #unwrap %~ M.filterWithKey (\k _v -> k `M.member` expected.unwrap)
    noop _expected actual = actual
    whenJust m f = maybe (return ()) f m
