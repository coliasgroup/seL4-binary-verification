{-# OPTIONS_GHC -Wno-orphans #-}

module BV.System.Scratch.Local
    ( runScratch
    ) where

import BV.Core.Types
import BV.Logging
import BV.System.Backend.Local
import BV.System.Cache
import BV.System.EvalStages
import BV.System.Frontend
import BV.System.SeL4
import BV.System.WithFingerprints
import BV.TargetDir

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Optics
import System.Exit (die)
import System.IO (BufferMode (LineBuffering), IOMode (WriteMode), hSetBuffering,
                  stderr, withFile)

runScratch :: LocalBackendConfig -> TargetDir -> FilePath -> FilePath -> IO ()
runScratch config targetDir logDst mismatchDumpDir = do
    withFile logDst WriteMode $ \fileHandle -> do
        hSetBuffering fileHandle LineBuffering
        let output entry = do
                simpleLogOutput formatLogEntryText LevelInfo stderr entry
                simpleLogOutput formatLogEntryText LevelDebug fileHandle entry
        flip runLoggingWithContextT output $
            flip runCacheT trivialCacheContext $
                 run
  where
    run = do
        input <- liftIO $ readStagesInput defaultSeL4AsmFunctionFilter targetDir
        checks <- evalStages ctx input
        report <- localBackend config (adornWithFingerprints checks)
        let failed = M.mapMaybe (preview _Left) report.unwrap
        if M.null failed
            then do
                liftIO $ putStrLn "All checks passed"
            else do
                forM_ (M.toAscList failed) $ \(pairingId, err) -> liftIO $ do
                    putStrLn $ "Check failure for " <> prettyPairingId pairingId <> ": " <> prettySMTProofCheckError err
                liftIO $ die "Some checks failed"
    ctx = EvalStagesContext
        { force = True
        , dumpTargetDir = Nothing
        , referenceTargetDir = Just targetDir
        , mismatchDumpDir = Just mismatchDumpDir
        }
