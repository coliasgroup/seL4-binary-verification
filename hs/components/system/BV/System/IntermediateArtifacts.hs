{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BV.System.IntermediateArtifacts
    ( RegisterIntermediateArtifactsT
    , RegisterIntermediateArtifactsTInnerContext (..)
    , runRegisterIntermediateArtifactsT
    ) where

import BV.ConcreteSyntax
import BV.Core.GluedStages
import BV.Core.Types
import BV.TargetDir

import Control.DeepSeq (force)
import Control.Monad (when)
import Control.Monad.Free (liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans.Free.Church (FT, iterT)
import Data.Function (applyWhen)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics (Generic)
import Optics.Core
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

data RegisterIntermediateArtifactsDSL a
  = RegisterIntermediateArtifact IntermediateArtifact a
  deriving (Functor, Generic)

instance Monad m => MonadRegisterIntermediateArtifacts (FT RegisterIntermediateArtifactsDSL m) where
    registerIntermediateArtifact x = liftF $ RegisterIntermediateArtifact x ()

type RegisterIntermediateArtifactsT m = FT RegisterIntermediateArtifactsDSL (RegisterIntermediateArtifactsTInner m)

data RegisterIntermediateArtifactsTInnerContext
  = RegisterIntermediateArtifactsTInnerContext
      { force :: Bool
      , dumpTargetDir :: Maybe TargetDir
      , referenceTargetDir :: Maybe TargetDir
      , mismatchDumpDir :: Maybe FilePath
      }
  deriving (Eq, Generic, Ord, Show)

type RegisterIntermediateArtifactsTInner m = ReaderT RegisterIntermediateArtifactsTInnerContext m

runRegisterIntermediateArtifactsT
    :: (Monad m, MonadLogger m, MonadIO m, MonadFail m)
    => RegisterIntermediateArtifactsT m a
    -> RegisterIntermediateArtifactsTInner m a
runRegisterIntermediateArtifactsT = iterT $ \(RegisterIntermediateArtifact artifact m) -> do
        ctx <- ask
        let register f file actual = do
                whenJust ctx.dumpTargetDir $ \dumpTargetDir -> do
                    logInfoN $ "Dumping " <> T.pack file.relativePath
                    liftIO $ writeTargetDirFile dumpTargetDir file actual
                whenJust ctx.referenceTargetDir $ \referenceTargetDir -> do
                    expected <- liftIO $ readTargetDirFile referenceTargetDir file
                    let actual' = f expected actual
                    when (force actual' /= force expected) $ do
                        logErrorN $ "Intermediate artifact mismatch for " <> T.pack file.relativePath
                        whenJust ctx.mismatchDumpDir $ \mismatchDumpDir -> do
                            let d = mismatchDumpDir </> file.relativePath
                            logErrorN $ "Writing mismatch to " <> T.pack d
                            liftIO $ do
                                createDirectoryIfMissing True d
                                writeBVFile (d </> "actual.txt") actual'
                                writeBVFile (d </> "expected.txt") expected
                        fail "Intermediate artifact mismatch"
        case applyWhen ctx.force force artifact of
            IntermediateArtifactFunctions a -> register noop targetDirFiles.functions a
            IntermediateArtifactPairings a -> register noop targetDirFiles.pairings a
            IntermediateArtifactProblems a -> register filterProblems targetDirFiles.problems a
            IntermediateArtifactFlattenedProofChecks a -> register noop targetDirFiles.proofChecks a
            IntermediateArtifactFlattenedSMTProofChecks a -> register noop targetDirFiles.smtProofChecks a
            -- IntermediateArtifactFlattenedSMTProofChecks a -> return ()
        m
  where
    filterProblems expected actual = actual & #unwrap %~ M.filterWithKey (\k _v -> k `M.member` expected.unwrap)
    noop _expected actual = actual
    whenJust m f = maybe (return ()) f m
