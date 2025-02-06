{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main
    ( main
    ) where

import BV.Core.ExecuteSMTProofChecks
import BV.SMTLIB2.Monad
import BV.System.Backend.Core
import BV.System.Backend.Local
import BV.System.Scratch.Local
import BV.System.SolversConfig
import BV.Test.Utils

import System.FilePath ((</>))

main :: IO ()
main = runScratch
    config
    -- testSeL4TargetDirSmall
    -- testSeL4TargetDirSmallTraceOfflineOnly
    -- testSeL4TargetDirFocusedTrace
    testSeL4TargetDirBig
    (tmpDir </> "logs/check-scratch.log2.txt")
    (tmpDir </> "mismatch")

config :: LocalBackendConfig
config = LocalBackendConfig
    { numCores = 16
    -- { numCores = 1
    , backendCoreConfig = BackendCoreConfig
        { solversConfig
        }
    }

solversConfig :: SolversConfig
solversConfig = SolversConfig
    { online = OnlineSolverConfig
        { command = ["yices-smt2", "--incremental"]
        , modelConfig = ModelConfig { memoryMode = SolverMemoryModeWord8 }
        , timeout = solverTimeoutFromSeconds 30
        }
    , offline = OfflineSolversConfig
        { groups =
            [ f ["yices-smt2"]
            , f ["bitwuzla"]
            -- , f ["cvc5", "--lang", "smt"]
            -- [ f ["cvc5", "--lang", "smt"]
            -- , f ["z3", "-smt2", "-in"]
            -- [ f ["z3", "-smt2", "-in"]
            -- [ f ["mathsat", "-input=smt2"]
            -- [ f ["sonolar", "--input-format=smtlib2"]
            ]
        , timeout = solverTimeoutFromSeconds  6000
        }
    }
  where
    f command@(commandName:_) = OfflineSolverGroupConfig
        { commandName
        , command
        , scopes = allSolverScopes
        -- , scopes = [SolverScopeHyp]
        -- , scopes = [SolverScopeAll]
        , modelConfigs = allModelConfigs
        -- , modelConfigs = [ModelConfig { memoryMode = SolverMemoryModeWord8 }]
        -- , modelConfigs = [ModelConfig { memoryMode = SolverMemoryModeWord32 }]
        }
