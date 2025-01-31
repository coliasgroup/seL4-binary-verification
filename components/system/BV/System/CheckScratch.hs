module BV.System.CheckScratch
    ( runScratch
    ) where

import BV.Core.ExecuteSMTProofChecks
import BV.System.CheckFrontend
import BV.System.SolversConfig
import BV.TargetDir

-- executeChecksConfig :: ExecuteChecksConfig
-- executeChecksConfig = ExecuteChecksConfig
--     { numCores = 12
--     , solversConfig
--     }

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

runScratch :: TargetDir -> IO ()
runScratch targetDir = return ()
