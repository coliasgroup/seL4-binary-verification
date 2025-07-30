{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BV.System.EvalStages
    ( EvalStagesContext (..)
    , evalStages
    ) where

import BV.ConcreteSyntax
import BV.Core.Prelude
import BV.Core.Types
import BV.Logging
import BV.System.Core
import BV.TargetDir

import Control.DeepSeq (NFData, deepseq, force, rnf)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function (applyWhen)
import qualified Data.Map as M
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import Optics
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Printf (printf)

data EvalStagesContext
  = EvalStagesContext
      { forceAll :: Bool
      , forceFingerprints :: Bool
      , dumpTargetDir :: Maybe TargetDir
      , referenceTargetDir :: Maybe TargetDir
      , mismatchDumpDir :: Maybe FilePath
      }
  deriving (Eq, Generic, Ord, Show)

evalStages
    :: forall m. (MonadIO m, MonadFail m, MonadLogger m)
    => EvalStagesContext
    -> StagesInput
    -> m Checks
evalStages ctx input = do

    logWarn $
        "Unhandled inline assembly functions (C side): " ++ show (map (.unwrap) output.unhandledInlineAssemblyFunctions)
    logWarn $
        "Unhandled instrcution functions (ASM side): " ++ show (map (.unwrap) output.unhandledInstructionFunctions)

    unless registerIsNoop $ do
        logInfo "Registering functions"
        register filterFunctions targetDirFiles.functions output.intermediate.functions
        logInfo "Registering pairings"
        register noop targetDirFiles.pairings output.intermediate.pairings
        logInfo "Registering problems"
        register filterProblems targetDirFiles.problems output.intermediate.problems
        logInfo "Registering proof checks"
        register noop targetDirFiles.proofChecks output.intermediate.compatProofChecks
        logInfo "Registering SMT proof checks"
        register noop targetDirFiles.smtProofChecks output.intermediate.compatSMTProofChecks
        logInfo "Registered all intermediate artifacts"


    let checks = elaborateChecks output.checks

    when ctx.forceFingerprints $ do
        logInfo $ printf "Enumerating check groups"
        !_ <- return $ rnf $ (foldMap M.keys (M.elems checks.unwrap) :: [CheckGroupFingerprint])
        logInfo "Done enumerating check groups"

    return checks

  where

    output = stages input

    registerIsNoop = not $ ctx.forceAll || isJust ctx.dumpTargetDir || isJust ctx.referenceTargetDir

    register :: forall a c. (Eq a, NFData a, ReadBVFile c a, WriteBVFile c a) => (a -> a -> a) -> TargetDirFile a -> a -> m ()
    register f file actual = applyWhen ctx.forceAll (deepseq actual) $ do
        whenJust_ ctx.dumpTargetDir $ \dumpTargetDir -> do
            logInfo $ "Dumping " ++ file.relativePath
            liftIO $ writeTargetDirFile dumpTargetDir file actual
        whenJust_ ctx.referenceTargetDir $ \referenceTargetDir -> do
            expected <- liftIO $ readTargetDirFile referenceTargetDir file
            let actual' = f expected actual
            when (maybeForce actual' /= maybeForce expected) $ do
                logError $ "Intermediate artifact mismatch for " ++ file.relativePath
                whenJust_ ctx.mismatchDumpDir $ \mismatchDumpDir -> do
                    let d = mismatchDumpDir </> file.relativePath
                    logInfo $ "Writing mismatch to " ++ d
                    liftIO $ do
                        createDirectoryIfMissing True d
                        writeBVFile (d </> "actual.txt") actual'
                        writeBVFile (d </> "expected.txt") expected
                fail "Intermediate artifact mismatch"

    maybeForce :: forall a. NFData a => a -> a
    maybeForce = applyWhen ctx.forceAll force

    filterFunctions expected actual = actual & #functions %~ M.filterWithKey (\k _v -> k `M.member` expected.functions)
    filterProblems expected actual = actual & #unwrap %~ M.filterWithKey (\k _v -> k `M.member` expected.unwrap)

    noop _expected actual = actual

    whenJust_ m f = maybe (return ()) f m
