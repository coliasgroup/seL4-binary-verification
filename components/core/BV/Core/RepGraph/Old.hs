module BV.Core.RepGraph.Old
    ( module BV.Core.RepGraph.Old.AsmStackRep
    , module BV.Core.RepGraph.Old.Base
    , module BV.Core.RepGraph.Old.FunAsserts
    , module BV.Core.RepGraph.Old.InterpretHyp
    , ForTag
    , FunCallInfo (..)
    , MonadRepGraph (..)
    , MonadRepGraphDefaultHelper (..)
    , MonadRepGraphFlatten
    , MonadRepGraphForTag (..)
    , MonadRepGraphSolver (..)
    , MonadRepGraphSolverSend (..)
    , Name (..)
    , PcEnv (..)
    , SolverExpr
    , SolverExprContext (..)
    , addPValidDomAssertions
    , askLoopData
    , askNodeGraph
    , askProblem
    , convertExprNotSplit
    , convertSolverExpr
    , getFunCallInfo
    , getFunCallInfoRaw
    , getModelExprs
    , getModelVars
    , getNodePcEnv
    , getNodePcEnvWithTag
    , getPc
    , getPcWithTag
    , isUnreachable
    , runForTag
    , tryGetNodePcEnv
    , withEnv
    , withoutEnv
    ) where

import BV.Core.RepGraph.Old.AsmStackRep
import BV.Core.RepGraph.Old.Base
import BV.Core.RepGraph.Old.Core
import BV.Core.RepGraph.Old.FunAsserts
import BV.Core.RepGraph.Old.InterpretHyp
import BV.Core.RepGraph.Old.Solver

import BV.Core.Types (SExprWithPlaceholders, Visit)
import BV.Core.Types.Extras

import Data.Maybe (fromJust)

convertSolverExpr :: MonadRepGraphSolver m => SolverExpr -> m SExprWithPlaceholders
convertSolverExpr expr = withoutEnv $ convertExprNotSplit $ castExpr expr

isUnreachable :: MonadRepGraph t m => Visit -> ForTag t m SExprWithPlaceholders
isUnreachable visit = do
    pcEnv <- fromJust <$> getNodePcEnv visit
    withEnv pcEnv.env $ convertExprNotSplit $ notE pcEnv.pc

class MonadRepGraphFlatten m
