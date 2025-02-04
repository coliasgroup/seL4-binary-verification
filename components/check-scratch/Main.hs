{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main
    ( main
    ) where

import BV.System.Backend.Core
import BV.System.Backend.Local
import BV.System.Scratch.Local
import BV.System.SolversConfig
import BV.Test.Utils

import System.FilePath ((</>))

main :: IO ()
main = runScratch
    config
    testSeL4TargetDirSmall
    -- testSeL4TargetDirBig
    (tmpDir </> "check-scratch.log.txt")
    (tmpDir </> "mismatch")

config :: LocalBackendConfig
config = LocalBackendConfig
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
        [ f ["yices-smt2"]
        , f ["bitwuzla"]
        , f ["cvc5", "--lang", "smt"]
        -- [ f ["z3", "-smt2", "-in"]
        -- [ f ["mathsat", "-input=smt2"]
        -- [ f ["sonolar", "--input-format=smtlib2"]
        ]
    , offlineTimeout = 6000
    }
  where
    f command@(commandName:_) = OfflineSolverGroupConfig
        { commandName
        , command
        , memoryModes = allSolverMemoryModes
        , scopes = allSolverScopes
        }
