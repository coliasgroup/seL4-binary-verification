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

runRegisterIntermediateArtifactsT
    :: (Monad m, MonadLogger m, MonadIO m, MonadFail m)
    => RegisterIntermediateArtifactsT m a
    -> RegisterIntermediateArtifactsTInner m a
runRegisterIntermediateArtifactsT = iterT $ \case
    RegisterIntermediateArtifact artifact m -> do
        ctx <- ask
        let check f read dumpDst actual = do
                expected <- liftIO (read ctx.targetDir) >>= \case
                    Left err -> fail err -- TODO
                    Right x -> return x
                let actual' = f expected actual
                when (force actual' /= force expected) $ do
                    let d = ctx.mismatchDumpDir </> dumpDst
                    logErrorN $ "intermediate artifact mismatch, writing to " <> T.pack d
                    liftIO $ do
                        createDirectoryIfMissing True d
                        writeBVFile (d </> "actual.txt") actual'
                        writeBVFile (d </> "expected.txt") expected
                    fail . T.unpack $ "intermediate artifact mismatch, wrote to " <> T.pack d
        let foo :: Problems -> Problems -> Problems
            foo exp act = act & #unwrap %~ M.filterWithKey (\k _v -> k `M.member` exp.unwrap)
        case artifact of
            IntermediateArtifactFunctions a -> check (const id) readFunctions "functions" a
            IntermediateArtifactPairings a -> check (const id) readPairings "pairings" a
            IntermediateArtifactProblems a -> check foo readProblems "problems" a
            IntermediateArtifactFlattenedProofChecks a -> check (const id) readProofChecks "proof-checks" a
            IntermediateArtifactFlattenedSMTProofChecks a -> check (const id) readSMTProofChecks "smt-proof-checks" a
        m
