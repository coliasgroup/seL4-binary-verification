{-# OPTIONS_GHC -Wno-orphans #-}

module BV.System.LocalCheckBackend.Scratch
    ( runScratch
    ) where

import BV.Core.AdornProofScript
import BV.Core.ExecuteSMTProofChecks
import BV.Core.GluedStages
import BV.Core.Types
import BV.System.CheckFrontend
import BV.System.IntermediateArtifacts
import BV.System.LocalCheckBackend
import BV.System.LocalCheckBackend.Cache
import BV.System.SeL4
import BV.System.SolversConfig
import BV.TargetDir

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (MonadLogger (monadLoggerLog), runStderrLoggingT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free.Church (FT)
import qualified Data.Map as M
import Optics

config :: LocalCheckBackendConfig
config = LocalCheckBackendConfig
    { numCores = 12
    , solversConfig
    }

solversConfig :: SolversConfig
solversConfig = SolversConfig
    { online = OnlineSolverConfig
        { command = ["yices-smt2", "--incremental"]
        , memoryMode = SolverMemoryModeWord8
        }
    , onlineTimeout = 30
    , offline =
        [ OfflineSolverGroupConfig
            { command = ["yices-smt2"]
            , memoryModes = allSolverMemoryModes
            , scopes = allSolverScopes
            }
        -- , OfflineSolverGroupConfig
        --     { command = ["z3", "-smt2", "-in"]
        --     , memoryModes = allSolverMemoryModes
        --     , scopes = allSolverScopes
        --     }
        -- , OfflineSolverGroupConfig
        --     { command = ["cvc5", "--lang", "smt"]
        --     , memoryModes = allSolverMemoryModes
        --     , scopes = allSolverScopes
        --     }
        -- , OfflineSolverGroupConfig
        --     { command = ["mathsat", "-input=smt2"]
        --     , memoryModes = allSolverMemoryModes
        --     , scopes = allSolverScopes
        --     }
        ]
    , offlineTimeout = 6000
    }

runScratch :: TargetDir -> FilePath -> IO ()
runScratch targetDir mismatchDumpDir =
    runStderrLoggingT $
        runReaderT (runLocalCheckCacheT go) trivialLocalCheckCacheContext
  where
    go = do
        input <- liftIO $ readGluedStagesInput defaultSeL4AsmFunctionFilter targetDir
        let ctx = RegisterIntermediateArtifactContext
                { force = True
                , dumpTargetDir = Nothing
                , referenceTargetDir = Just targetDir
                , mismatchDumpDir = Just mismatchDumpDir
                }
        structuredChecks <- gluedStages (registerIntermediateArtifactWith ctx) input
        let checks = flattenSMTProofChecks (adornSMTProofChecksWithDescriptions structuredChecks)
        report <- localCheckBackend config checks
        let brief = M.mapMaybe (preview _Left) report.unwrap
        forM_ (M.toAscList brief) $ \(pairingId, err) -> liftIO $ do
            putStrLn $ "Check failure for " <> prettyPairingId pairingId <> ": " <> show err

--

-- TODO HACK
instance (MonadLogger m, Functor f) => MonadLogger (FT f m) where
    monadLoggerLog loc source level msg = lift $ monadLoggerLog loc source level msg
