module BV.Core.GraphSlice.Old
    ( AsmRefineGraphSliceInput (..)
    , ExprEnv
    , FlatExpr
    , FunCallInfo (..)
    , GraphSliceHooks (preEmitCallNodeHook)
    , GraphSliceInput (..)
    , GraphSliceT
    , GraphSliceTaggedT
    , MonadGraphSliceSendSExpr (..)
    , PcEnv (..)
    , addAccumulatedAssertions
    , askCont
    , askLoopData
    , askNodeGraph
    , askProblem
    , askTag
    , askWithTag
    , asmRefineGraphSliceHooks
    , assertExpr
    , convertExpr
    , defaultGraphSliceHooks
    , flattenExpr
    , getFunCallInfo
    , getInductVar
    , getNodePcEnv
    , getNodePcEnvWithTag
    , getPc
    , getPcWithTag
    , instEqWithEnvs
    , interpretHyp
    , interpretHypImps
    , liftUntagged
    , runAsmRefineGraphSliceT
    , runGraphSliceT
    , runGraphSliceTStep
    , runTagged
    , tryGetNodePcEnv
    ) where

import BV.Core.GraphSlice.Old.Core
import BV.Core.GraphSlice.Old.InterpretHyp
import BV.Core.GraphSlice.Old.Solver

import BV.Core.GraphSlice.New (AsmRefineGraphSliceInput (..), FlatExpr,
                               GraphSliceInput (..))
import BV.Core.GraphSlice.New.Common

import BV.Core.Types
import BV.Core.Types.Extras

import Data.Foldable (toList)
import qualified Data.Map as M

runGraphSliceT
    :: (Tag t, MonadGraphSliceSendSExpr m)
    => GraphSliceHooks t m
    -> GraphSliceInput t
    -> GraphSliceT t m a
    -> m a
runGraphSliceT hooks input =
      runGraphSliceSolverTStep structs input.rodata
    . runGraphSliceTStep input.problem hooks
  where
    structs = (M.!) $ M.unionsWith undefined $
        rodataStructsOf input.rodata : toList input.structs

runAsmRefineGraphSliceT
    :: MonadGraphSliceSendSExpr m
    => AsmRefineGraphSliceInput
    -> GraphSliceT AsmRefineTag m a
    -> m a
runAsmRefineGraphSliceT input = runGraphSliceT hooks input.repGraphInput
  where
    argRenames =
        problemArgRenames input.repGraphInput.problem $
            input.lookupSig <$>
                withTags (pairingIdOfProblem input.repGraphInput.problem)
    hooks = asmRefineGraphSliceHooks input.lookupSig input.pairings argRenames

--

assertExpr :: (Tag t, MonadGraphSliceSendSExpr m) => FlatExpr -> GraphSliceT t m ()
assertExpr = liftInner . assertFact

convertExpr :: (Tag t, MonadGraphSliceSendSExpr m) => FlatExpr -> GraphSliceT t m SExprWithPlaceholders
convertExpr = liftInner . convertExprNotSplit

getPcWithTag :: (Tag t, MonadGraphSliceSendSExpr m) => WithTag t Visit -> GraphSliceT t m FlatExpr
getPcWithTag (WithTag tag visit) = fmap castExpr $ runTagged tag $ getPc visit

getNodePcEnvWithTag :: (Tag t, MonadGraphSliceSendSExpr m) => WithTag t Visit -> GraphSliceT t m (Maybe PcEnv)
getNodePcEnvWithTag (WithTag tag visit) = runTagged tag $ getNodePcEnv visit

addAccumulatedAssertions :: (Tag t, MonadGraphSliceSendSExpr m) => GraphSliceT t m ()
addAccumulatedAssertions = do
    liftInner $ sendAccumulatedSolverAssertions
