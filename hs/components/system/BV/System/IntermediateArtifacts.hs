{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BV.System.IntermediateArtifacts
    ( RegisterIntermediateArtifactsT
    , RegisterIntermediateArtifactsTInnerContext (..)
    , checkRegisterIntermediateArtifactsT
    ) where

import BV.ConcreteSyntax
import BV.Core.GluedStages
import BV.Core.Types
import BV.TargetDir

import Control.DeepSeq (force)
import Control.Monad (when)
import Control.Monad.Free (liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logErrorN)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans.Free.Church (FT, iterT)
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
      { targetDir :: TargetDir
      , mismatchDumpDir :: FilePath
      }
  deriving (Eq, Generic, Ord, Show)

type RegisterIntermediateArtifactsTInner m = ReaderT RegisterIntermediateArtifactsTInnerContext m

checkRegisterIntermediateArtifactsT
    :: (Monad m, MonadLogger m, MonadIO m, MonadFail m)
    => RegisterIntermediateArtifactsT m a
    -> RegisterIntermediateArtifactsTInner m a
checkRegisterIntermediateArtifactsT = iterT $ \case
    RegisterIntermediateArtifact artifact m -> do
        ctx <- ask
        let check f file actual = do
                expected <- liftIO $ readTargetDirFile ctx.targetDir file
                let actual' = f expected actual
                when (force actual' /= force expected) $ do
                    let d = ctx.mismatchDumpDir </> file.relativePath
                    logErrorN $ "Intermediate artifact mismatch, writing to " <> T.pack d
                    liftIO $ do
                        createDirectoryIfMissing True d
                        writeBVFile (d </> "actual.txt") actual'
                        writeBVFile (d </> "expected.txt") expected
                    fail . T.unpack $ "Intermediate artifact mismatch, wrote to " <> T.pack d
        let filterProblems expected actual = actual & #unwrap %~ M.filterWithKey (\k _v -> k `M.member` expected.unwrap)
        let noop _expected actual = actual
        case artifact of
            IntermediateArtifactFunctions a -> check noop targetDirFiles.functions a
            IntermediateArtifactPairings a -> check noop targetDirFiles.pairings a
            IntermediateArtifactProblems a -> check filterProblems targetDirFiles.problems a
            IntermediateArtifactFlattenedProofChecks a -> check noop targetDirFiles.proofChecks a
            IntermediateArtifactFlattenedSMTProofChecks a -> check noop targetDirFiles.smtProofChecks a
            -- IntermediateArtifactFlattenedSMTProofChecks a -> return ()
        m
