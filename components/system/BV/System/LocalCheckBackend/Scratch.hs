{-# OPTIONS_GHC -Wno-orphans #-}

module BV.System.LocalCheckBackend.Scratch
    ( runScratch
    ) where

import BV.Core.Types
import BV.System.CheckFrontend
import BV.System.EvalStages
import BV.System.LocalCheckBackend
import BV.System.LocalCheckBackend.Cache
import BV.System.SeL4
import BV.TargetDir

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (runReaderT)
import qualified Data.Map as M
import Optics
import System.Exit (die)

runScratch :: LocalCheckBackendConfig -> TargetDir -> FilePath -> IO ()
runScratch config targetDir mismatchDumpDir =
    runStderrLoggingT $
        runReaderT (runLocalCheckCacheT go) trivialLocalCheckCacheContext
  where
    go = do
        input <- liftIO $ readStagesInput defaultSeL4AsmFunctionFilter targetDir
        checks <- evalStages ctx input
        report <- localCheckBackend config checks
        let failed = M.mapMaybe (preview _Left) report.unwrap
        unless (M.null failed) $ do
            forM_ (M.toAscList failed) $ \(pairingId, err) -> liftIO $ do
                putStrLn $ "Check failure for " <> prettyPairingId pairingId <> ": " <> show err
            liftIO $ die "Some checks failed"
    ctx = EvalStagesContext
        { force = True
        , dumpTargetDir = Nothing
        , referenceTargetDir = Just targetDir
        , mismatchDumpDir = Just mismatchDumpDir
        }
