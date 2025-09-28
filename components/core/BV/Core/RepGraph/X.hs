module BV.Core.RepGraph.X
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

import BV.Core.RepGraph.X.Core
import BV.Core.RepGraph.X.Solver

import BV.Core.RepGraph.New (FlatExpr)

import BV.Core.Types
import BV.Core.Types.Extras
import BV.SMTLIB2 (SExpr)

import BV.Core.RepGraph.X.Common
import Control.Monad ((>=>))
import Data.Foldable (toList)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Optics

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
      runRepGraphSolverTStep structs input.rodata
    . runRepGraphTStep input.problem hooks
  where
    structs = (M.!) $ M.unionsWith undefined $
        rodataStructsOf input.rodata : toList input.structs

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

--

flattenExpr :: ExprEnv -> GraphExpr -> FlatExpr
flattenExpr = flip go
  where
    go = traverseOf (exprArgs % traversed) go >=> \expr -> case expr.value of
        ExprValueVar name -> \env -> Expr expr.ty $ ExprValueSMTExpr (env ! NameTy name expr.ty)
        _ -> return expr

assertExpr :: (Tag t, MonadRepGraphSendSExpr m) => FlatExpr -> RepGraphT t m ()
assertExpr = liftInner . withoutEnv . assertFact . castExpr

convertExpr :: (Tag t, MonadRepGraphSendSExpr m) => FlatExpr -> RepGraphT t m SExprWithPlaceholders
convertExpr expr = liftInner $ withoutEnv $ convertExprNotSplit $ castExpr expr

getPcWithTag :: (Tag t, MonadRepGraphSendSExpr m) => WithTag t Visit -> RepGraphT t m FlatExpr
getPcWithTag (WithTag tag visit) = fmap castExpr $ runTagged tag $ getPc visit

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
    liftInner $ withEnv pcEnv.env $ convertExprNotSplit $ notE pcEnv.pc

--

addPValidDomAssertions :: MonadRepGraphSendSExpr m => RepGraphT t m ()
addPValidDomAssertions = do
    liftInner addPValidDomAssertions'
