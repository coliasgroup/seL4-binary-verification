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
    , addPValidDomAssertions
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
    , getEvalModel
    , getFunCallInfo
    , getInductVar
    , getModelRequest
    , getNodePcEnv
    , getNodePcEnvWithTag
    , getPc
    , getPcWithTag
    , instEqWithEnvs
    , instEqWithEnvsCompat
    , isUnreachableCompat
    , liftUntagged
    , runAsmRefineGraphSliceT
    , runGraphSliceT
    , runGraphSliceTStep
    , runTagged
    , tryGetNodePcEnv
    ) where

import BV.Core.GraphSlice.Old.Core
import BV.Core.GraphSlice.Old.Solver

import BV.Core.GraphSlice.New (AsmRefineGraphSliceInput (..), FlatExpr,
                               GraphSliceInput (..))
import BV.Core.GraphSlice.New.Common

import BV.Core.Types
import BV.Core.Types.Extras
import BV.SMTLIB2 (SExpr)

import Control.Monad ((>=>))
import Data.Foldable (toList)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Optics

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

flattenExpr :: ExprEnv -> GraphExpr -> FlatExpr
flattenExpr = flip go
  where
    go = traverseOf (exprArgs % traversed) go >=> \expr -> case expr.value of
        ExprValueVar name -> \env -> Expr expr.ty $ ExprValueSMTExpr (env ! NameTy name expr.ty)
        _ -> return expr

assertExpr :: (Tag t, MonadGraphSliceSendSExpr m) => FlatExpr -> GraphSliceT t m ()
assertExpr = liftInner . withoutEnv . assertFact . castExpr

convertExpr :: (Tag t, MonadGraphSliceSendSExpr m) => FlatExpr -> GraphSliceT t m SExprWithPlaceholders
convertExpr expr = liftInner $ withoutEnv $ convertExprNotSplit $ castExpr expr

getPcWithTag :: (Tag t, MonadGraphSliceSendSExpr m) => WithTag t Visit -> GraphSliceT t m FlatExpr
getPcWithTag (WithTag tag visit) = fmap castExpr $ runTagged tag $ getPc visit

getNodePcEnvWithTag :: (Tag t, MonadGraphSliceSendSExpr m) => WithTag t Visit -> GraphSliceT t m (Maybe PcEnv)
getNodePcEnvWithTag (WithTag tag visit) = runTagged tag $ getNodePcEnv visit

--

type ModelRequest = [String]

newtype ModelResponse
  = ModelResponse { unwrap :: M.Map String SExpr }
  deriving (Generic, Show)

getModelRequest :: (Tag t, MonadGraphSliceSendSExpr m) => GraphSliceT t m ModelRequest
getModelRequest = undefined

getEvalModel :: (Tag t, MonadGraphSliceSendSExpr m) => GraphSliceT t m (ModelResponse -> FlatExpr -> FlatExpr)
getEvalModel = undefined
    -- modelResp expr

-- newtype Model = Model
--     { unwrap :: M.Map FlatExpr FlatExpr
--     }
--   deriving (Generic, Show)

--

isUnreachableCompat :: (Tag t, MonadGraphSliceSendSExpr m) => Visit -> GraphSliceTaggedT t m SExprWithPlaceholders
isUnreachableCompat visit = do
    pcEnv <- fromJust <$> getNodePcEnv visit
    liftInner $ withEnv pcEnv.env $ convertExprNotSplit $ notE pcEnv.pc

instEqWithEnvsCompat :: (Tag t, MonadGraphSliceSendSExpr m) => (GraphExpr, ExprEnv) -> (GraphExpr, ExprEnv) -> GraphSliceT t m FlatExpr
instEqWithEnvsCompat = instEqWithEnvs

--

addPValidDomAssertions :: MonadGraphSliceSendSExpr m => GraphSliceT t m ()
addPValidDomAssertions = do
    liftInner addPValidDomAssertions'
