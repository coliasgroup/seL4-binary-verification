{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main
    ( main
    ) where

import BV.Core.DecorateProofScript
import BV.Core.ModelConfig
import BV.Core.Prelude
import BV.Core.Stages
import BV.Core.Types
import BV.Core.Types.Extras.DebugShowExpr
import BV.Core.Types.Extras.Expr
import BV.Core.Types.Extras.Problem
import BV.Core.Types.Extras.Program (nodeAddrOf)
import BV.Core.Types.Extras.SExprWithPlaceholders
import BV.Logging
import BV.Search.Core.GraphSlice
import BV.Search.Core.Solver
import BV.Search.System.Core
import BV.SMTLIB2
import BV.SMTLIB2.Command
import BV.System.Core
import BV.TargetDir
import BV.Test.Utils
import BV.Test.Utils.Paths
import BV.Utils

import Control.Monad (unless, when)
import Control.Monad.Trans (lift)
import Data.Foldable (for_)
import Data.List (genericIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Optics
import System.FilePath ((</>))

import Debug.Trace (traceM, traceShowM)

logPath :: FilePath
logPath = defaultOutDir </> "logs" </> "x.txt"

main :: IO ()
main = withLoggingOpts loggingOpts $ withPushLogContext "x" $ do
    input <- lift $ seL4DefaultReadStagesInput $ TargetDir $ defaultTestTargetDir "big"
    let output = stages input
    let pairingId = thisPath.checkGroupPath.pairingId
    let problem = output.intermediate.problems.unwrap M.! pairingId
    let analysis = analyzeProblem problem
    let check = thisLookupCheck analysis thisPath output.intermediate.proofChecks
    let graphSliceInput = output.intermediate.graphSliceInput M.! pairingId
    logInfo $ show ("meta", check.meta)
    runGraphSliceSolverInteractSimple' solverConfig $ do
        runAsmRefineGraphSliceT graphSliceInput $ do
            goal <- interpretCheck check
            r <- testHyp goal
            traceShowM ("r", r)
    return ()

thisLookupCheck
    :: t ~ AsmRefineTag
    => ProblemAnalysis t
    -> CheckPath
    -> ProofChecks t ProofCheckDescription
    -> ProofCheck t ProofCheckDescription
thisLookupCheck analysis path =
      (`genericIndex` path.indexInGroup.unwrap)
    . fromJust
    . lookup path.checkGroupPath.checkIndices
    . prunedProofCheckGroups analysis
    . followProofScriptEdgePath path.checkGroupPath.proofScriptEdgePath
    . view (#unwrap % expectingAt path.checkGroupPath.pairingId % #root)

thisPath :: CheckPath
thisPath = CheckPath
    { checkGroupPath = CheckGroupPath
        { pairingId = byTagFrom $ Ident . \case
            C -> "Kernel_C." ++ name
            Asm -> name
        , proofScriptEdgePath = []
        , checkIndices = ProofCheckGroupCheckIndices $ S.fromList [0,1,2,3,4,5,6,7,8,9,10,11,12]
        , fingerprint = undefined
        }
    , indexInGroup = CheckIndexInGroup 0
    , fingerprint = undefined
    }
  where
    name =
        -- "cleanInvalidateL1Caches"
        "unmapPageTable"


splitDisjunction :: Expr c -> [Expr c]
splitDisjunction = go
  where
    go expr = case expr.value of
        ExprValueOp OpAnd args -> concatMap go args
        _ -> [expr]

--

loggingOpts :: LoggingOpts
loggingOpts = LoggingOpts
    { stderrLogOpts = LogOpts
        { level =
            LevelInfo
            -- LevelDebug
        , format = LogFormatHuman
        }
    , fileLogOpts = Just $ FileLogOpts
        { dst = logPath
        , logOpts = LogOpts
            { level =
                -- LevelDebug
                levelTrace
            , format =
                LogFormatText
                -- LogFormatHuman
            }
        }
    }

solverConfig :: OnlineSolverConfig
solverConfig = OnlineSolverConfig
    { command = SolverCommand
        { path = "yices-smt2"
        , args = ["--incremental"]
        }
    , modelConfig = ModelConfig
        { memoryMode = MemoryModeWord32
        }
    , timeout = solverTimeoutFromSeconds 30
    }
