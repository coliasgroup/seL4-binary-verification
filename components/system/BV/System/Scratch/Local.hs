{-# OPTIONS_GHC -Wno-orphans #-}

module BV.System.Scratch.Local
    ( runScratch
    ) where

import BV.Core.Types
import BV.System.Backend.Local
import BV.System.Cache
import BV.System.EvalStages
import BV.System.Frontend
import BV.System.SeL4
import BV.System.Utils.Logger
import BV.System.WithFingerprints
import BV.TargetDir

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel (LevelDebug, LevelInfo), defaultOutput,
                             runLoggingT)
import qualified Data.Map as M
import Optics
import System.Exit (die)
import System.IO (BufferMode (LineBuffering), IOMode (WriteMode), hSetBuffering,
                  stderr, withFile)

runScratch :: LocalBackendConfig -> TargetDir -> FilePath -> FilePath -> IO ()
runScratch config targetDir logDst mismatchDumpDir = do
    withFile logDst WriteMode $ \fileHandle -> do
        hSetBuffering fileHandle LineBuffering
        let output loc source level str = do
                when (filterLevelsBelow LevelInfo source level) $ do
                    defaultOutput stderr loc source level str
                when (filterLevelsBelow LevelDebug source level) $ do
                -- when (filterLevelsBelow levelTrace source level) $ do
                    defaultOutput fileHandle loc source level str
        flip (runLoggingT . runSimpleLoggingWithContextT) output $
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
                    putStrLn $ "Check failure for " <> prettyPairingId pairingId <> ": " <> show err
                liftIO $ die "Some checks failed"
    ctx = EvalStagesContext
        { force = True
        , dumpTargetDir = Nothing
        , referenceTargetDir = Just targetDir
        , mismatchDumpDir = Just mismatchDumpDir
        }
