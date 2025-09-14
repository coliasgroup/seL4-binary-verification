module BV.Core.RepGraph.New
    ( module BV.Core.RepGraph.New.AsmStackRep
    , module BV.Core.RepGraph.New.Base
    , module BV.Core.RepGraph.New.FunAsserts
    , module BV.Core.RepGraph.New.InterpretHyp
    , FlatExpr
    , FlatExprContext (..)
    , ForTag
    , FunCallInfo (..)
    , MonadRepGraph (..)
    , MonadRepGraphDefaultHelper (..)
    , MonadRepGraphFlatten (..)
    , MonadRepGraphForTag (..)
    , MonadRepGraphSolver (..)
    , MonadRepGraphSolverSend (..)
    , Name
    , PcEnv (..)
    , SolverExpr
    , SolverExprContext (..)
    , addPValidDomAssertions
    , askLoopData
    , askNodeGraph
    , askProblem
    , convertFlatExpr
    , convertSolverExpr
    , getFunCallInfo
    , getFunCallInfoRaw
      -- , getModelExprs
      -- , getModelVars
    , getNodePcEnv
    , getNodePcEnvWithTag
    , getPc
    , getPcWithTag
    , isUnreachable
    , runForTag
    , tryGetNodePcEnv
    , withEnv
    ) where

import BV.Core.RepGraph.New.AsmStackRep
import BV.Core.RepGraph.New.Base
import BV.Core.RepGraph.New.Core
import BV.Core.RepGraph.New.Flatten
import BV.Core.RepGraph.New.FunAsserts
import BV.Core.RepGraph.New.InterpretHyp
import BV.Core.RepGraph.New.Solver

import BV.Core.Types
import BV.Core.Types.Extras

import Data.Maybe (fromJust)

-- TODO
addPValidDomAssertions :: MonadRepGraphFlatten m => m ()
addPValidDomAssertions = do
    return ()

isUnreachable :: MonadRepGraph t m => Visit -> ForTag t m SExprWithPlaceholders
isUnreachable visit = do
    pcEnv <- fromJust <$> getNodePcEnv visit
    convertFlatExpr (notE pcEnv.pc) >>= convertSolverExpr

type Name = Ident
