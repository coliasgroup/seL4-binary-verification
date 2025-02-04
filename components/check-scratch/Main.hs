module Main
    ( main
    ) where

import BV.System.Backend.Core
import BV.System.Backend.Local
import BV.System.LocalCheckBackend.Scratch
import BV.System.SolversConfig
import BV.Test.Utils

import System.FilePath ((</>))

main :: IO ()
main = runScratch
    config
    testSeL4TargetDirSmall
    (tmpDir </> "check-scratch.log.txt")
    (tmpDir </> "mismatch")

config :: LocalCheckBackendConfig
config = LocalCheckBackendConfig
    { numCores = 12
    , backendCoreConfig = BackendCoreConfig
        { solversConfig
        }
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
