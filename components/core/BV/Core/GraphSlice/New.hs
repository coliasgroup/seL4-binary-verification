module BV.Core.GraphSlice.New
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
    , getEvalModel
    , getFunCallInfo
    , getInductVar
    , getModelRequest
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

import BV.Core.GraphSlice.New.Common
import BV.Core.GraphSlice.New.Flat
import BV.Core.GraphSlice.New.Flatten
import BV.Core.GraphSlice.New.InterpretHyp
import BV.Core.GraphSlice.New.SendFlatExprCommand
import BV.Core.GraphSlice.New.SendSolverExprCommand

import BV.Core.Types
import BV.Core.Types.Extras
import BV.SMTLIB2 (SExpr)

import Control.Monad ((>=>))
import Data.Foldable (toList)
import qualified Data.Map as M
import GHC.Generics (Generic)

data GraphSliceInput t
  = GraphSliceInput
      { structs :: ByTag t (M.Map Ident Struct)
      , rodata :: ROData
      , problem :: Problem t
      }
  deriving (Generic)

runGraphSliceT
    :: (Tag t, MonadGraphSliceSendSExpr m)
    => GraphSliceHooks t m
    -> GraphSliceInput t
    -> GraphSliceT t m a
    -> m a
runGraphSliceT hooks input =
      runGraphSliceSendSolverExprCommandTStep input.rodata
    . runGraphSliceSendFlatExprCommandTStep structs (rodataPtrsFromROData input.rodata)
    . runGraphSliceFlatTStep
    . runGraphSliceTStep input.problem hooks
  where
    structs = (M.!) $ M.unionsWith undefined $
        rodataStructsOf input.rodata : toList input.structs

rodataPtrsFromROData :: ROData -> [Expr c]
rodataPtrsFromROData rodata =
    [ pointerE (structT structName) (machineWordE range.addr)
    | (structName, range) <- rodataStructNamesOf rodata
    ]

data AsmRefineGraphSliceInput
  = AsmRefineGraphSliceInput
      { repGraphInput :: GraphSliceInput AsmRefineTag
      , lookupSig :: LookupFunctionSignature AsmRefineTag
      , pairings :: Pairings'
      }
  deriving (Generic)

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
assertExpr = liftInner . assertFlatExpr

convertExpr :: (Tag t, MonadGraphSliceSendSExpr m) => FlatExpr -> GraphSliceT t m SExprWithPlaceholders
convertExpr =
    liftInner . liftInner . convertFlatExpr
        >=> liftInner . liftInner . liftInner . convertSolverExpr

getPcWithTag :: (Tag t, MonadGraphSliceSendSExpr m) => WithTag t Visit -> GraphSliceT t m FlatExpr
getPcWithTag (WithTag tag visit) = runTagged tag $ getPc visit

getNodePcEnvWithTag :: (Tag t, MonadGraphSliceSendSExpr m) => WithTag t Visit -> GraphSliceT t m (Maybe PcEnv)
getNodePcEnvWithTag (WithTag tag visit) = runTagged tag $ getNodePcEnv visit

addAccumulatedAssertions :: (Tag t, MonadGraphSliceSendSExpr m) => GraphSliceT t m ()
addAccumulatedAssertions = do
    liftInner $ liftInner $ sendAccumulatedAssertionsInner

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
