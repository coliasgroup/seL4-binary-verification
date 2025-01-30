module BV.System.CheckScratch
    ( runScratch
    ) where

import BV.Core.ExecuteSMTProofChecks
import BV.System.Check
import BV.System.SolversConfig
import BV.TargetDir

executeChecksConfig :: ExecuteChecksConfig
executeChecksConfig = ExecuteChecksConfig
    { numCores = 12
    , solversConfig
    }

solversConfig :: SolversConfig
solversConfig = SolversConfig
    { online = OnlineSolverConfig
        { command = ["yices-smt2", "--incremental"]
        , memoryMode = SolverMemoryModeWord8
        }
    , offline =
        [ OfflineSolverConfig
            { command = ["yices-smt2"]
            , memoryModes = allSolverMemoryModes
            , scopes = allSolverScopes
            }
        -- , OfflineSolverConfig
        --     { command = ["z3", "-smt2", "-in"]
        --     , memoryModes = allSolverMemoryModes
        --     , scopes = allSolverScopes
        --     }
        -- , OfflineSolverConfig
        --     { command = ["cvc5", "--lang", "smt"]
        --     , memoryModes = allSolverMemoryModes
        --     , scopes = allSolverScopes
        --     }
        -- , OfflineSolverConfig
        --     { command = ["mathsat", "-input=smt2"]
        --     , memoryModes = allSolverMemoryModes
        --     , scopes = allSolverScopes
        --     }
        ]
    }

runScratch :: TargetDir -> IO ()
runScratch targetDir = return ()
