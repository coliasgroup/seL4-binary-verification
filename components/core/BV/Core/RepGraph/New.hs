module BV.Core.RepGraph.New
    ( AsmRefineRepGraphInput (..)
    , ExprEnv
    , FlatExpr
    , FunCallInfo (..)
    , MonadRepGraphSendSExpr (..)
    , PcEnv (..)
    , RepGraphHooks (preEmitCallNodeHook)
    , RepGraphInput (..)
    , RepGraphT
    , RepGraphTaggedT
    , addPValidDomAssertions
    , askCont
    , askLoopData
    , askNodeGraph
    , askProblem
    , askTag
    , askWithTag
    , asmRefineRepGraphHooks
    , assertExpr
    , convertExpr
    , defaultRepGraphHooks
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
    , isUnreachableCompat
    , liftUntagged
    , runAsmRefineRepGraphT
    , runRepGraphT
    , runRepGraphTStep
    , runTagged
    , tryGetNodePcEnv
    ) where

import BV.Core.RepGraph.New.Common
import BV.Core.RepGraph.New.Flat
import BV.Core.RepGraph.New.FlattenGraph
import BV.Core.RepGraph.New.SendFlatExprCommand
import BV.Core.RepGraph.New.SendSolverExprCommand

import BV.Core.Types
import BV.Core.Types.Extras
import BV.SMTLIB2 (SExpr)

import Control.Monad ((>=>))
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import GHC.Generics (Generic)

data RepGraphInput t
  = RepGraphInput
      { structs :: ByTag t (M.Map Ident Struct)
      , rodata :: ROData
      , problem :: Problem t
      }
  deriving (Generic)

runRepGraphT
    :: (Tag t, MonadRepGraphSendSExpr m)
    => RepGraphHooks t m
    -> RepGraphInput t
    -> RepGraphT t m a
    -> m a
runRepGraphT hooks input =
      runRepGraphSendSolverExprCommandTStep input.rodata
    . runRepGraphSendFlatExprCommandTStep structs (rodataPtrsFromROData input.rodata)
    . runRepGraphFlatTStep
    . runRepGraphTStep input.problem hooks
  where
    structs = (M.!) $ M.unionsWith undefined $
        rodataStructsOf input.rodata : toList input.structs

rodataPtrsFromROData :: ROData -> [Expr c]
rodataPtrsFromROData rodata =
    [ pointerE (structT structName) (machineWordE range.addr)
    | (structName, range) <- rodataStructNamesOf rodata
    ]

data AsmRefineRepGraphInput
  = AsmRefineRepGraphInput
      { repGraphInput :: RepGraphInput AsmRefineTag
      , lookupSig :: LookupFunctionSignature AsmRefineTag
      , pairings :: Pairings'
      }
  deriving (Generic)

runAsmRefineRepGraphT
    :: MonadRepGraphSendSExpr m
    => AsmRefineRepGraphInput
    -> RepGraphT AsmRefineTag m a
    -> m a
runAsmRefineRepGraphT input = runRepGraphT hooks input.repGraphInput
  where
    argRenames =
        problemArgRenames input.repGraphInput.problem $
            input.lookupSig <$>
                withTags (pairingIdOfProblem input.repGraphInput.problem)
    hooks = asmRefineRepGraphHooks input.lookupSig input.pairings argRenames

assertExpr :: (Tag t, MonadRepGraphSendSExpr m) => FlatExpr -> RepGraphT t m ()
assertExpr = liftInner . assertFlatExpr

convertExpr :: (Tag t, MonadRepGraphSendSExpr m) => FlatExpr -> RepGraphT t m SExprWithPlaceholders
convertExpr =
    liftInner . liftInner . convertFlatExpr
        >=> liftInner . liftInner . liftInner . convertSolverExpr

getPcWithTag :: (Tag t, MonadRepGraphSendSExpr m) => WithTag t Visit -> RepGraphT t m FlatExpr
getPcWithTag (WithTag tag visit) = runTagged tag $ getPc visit

getNodePcEnvWithTag :: (Tag t, MonadRepGraphSendSExpr m) => WithTag t Visit -> RepGraphT t m (Maybe PcEnv)
getNodePcEnvWithTag (WithTag tag visit) = runTagged tag $ getNodePcEnv visit

--

type ModelRequest = [String]

newtype ModelResponse
  = ModelResponse { unwrap :: M.Map String SExpr }
  deriving (Generic, Show)

getModelRequest :: (Tag t, MonadRepGraphSendSExpr m) => RepGraphT t m ModelRequest
getModelRequest = undefined

getEvalModel :: (Tag t, MonadRepGraphSendSExpr m) => RepGraphT t m (ModelResponse -> FlatExpr -> FlatExpr)
getEvalModel = undefined
    -- modelResp expr

-- newtype Model = Model
--     { unwrap :: M.Map FlatExpr FlatExpr
--     }
--   deriving (Generic, Show)

--

isUnreachableCompat :: (Tag t, MonadRepGraphSendSExpr m) => Visit -> RepGraphTaggedT t m SExprWithPlaceholders
isUnreachableCompat visit = do
    pcEnv <- fromJust <$> getNodePcEnv visit
    liftUntagged $ convertExpr $ notE pcEnv.pc
