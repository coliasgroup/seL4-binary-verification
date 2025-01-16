{-# LANGUAGE OverloadedStrings #-}

module BV.System.IntermediateArtifacts
    ( RegisterIntermediateArtifactsT
    , RegisterIntermediateArtifactsTInnerContext (..)
    , runRegisterIntermediateArtifactsT
    ) where

import Control.Monad (when)
import Control.Monad.Free (liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logErrorN)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans.Free.Church (FT, iterT)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TL
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import BV.ConcreteSyntax (BuildToFile (buildToFile))
import BV.Core.GluedStages
import BV.TargetDir

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
        let check read dumpDst actual = do
                expected <- liftIO (read ctx.targetDir) >>= \case
                    Left err -> fail err -- TODO
                    Right x -> return x
                when (actual /= expected) $ do
                    let d = ctx.mismatchDumpDir </> dumpDst
                    logErrorN $ "intermediate artifact mismatch, writing to " <> T.pack d
                    liftIO $ do
                        createDirectoryIfMissing True d
                        TL.writeFile (d </> "actual.txt") (TB.toLazyText (buildToFile actual))
                        TL.writeFile (d </> "expected.txt") (TB.toLazyText (buildToFile expected))
                    fail . T.unpack $ "intermediate artifact mismatch, wrote to " <> T.pack d
        case artifact of
            IntermediateArtifactFunctions a -> check readFunctions "functions" a
            IntermediateArtifactPairings a -> check readPairings "pairings" a
            IntermediateArtifactProblems a -> check readProblems "problems" a
            IntermediateArtifactProofChecks a -> check readProofChecks "proof-checks" a
            IntermediateArtifactSMTProofChecks a -> check readSMTProofChecks "smt-proof-checks" a
        m
