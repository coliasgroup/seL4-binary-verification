module BV.Core.GraphSlice.New
    ( AsmRefineGraphSliceInput (..)
    , ExprEnv
    , FlatExpr
    , FunCallInfo (..)
    , GraphSliceHooks (preEmitCallNodeHook)
    , GraphSliceInput (..)
    , GraphSliceT
    , GraphSliceTaggedT
    , MonadGraphSliceGetSExprValue (..)
    , MonadGraphSliceSendSExpr (..)
    , PcEnv (..)
    , addAccumulatedAssertions
    , askContVisit
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
    , getFlatExprValue
    , getFunCallInfo
    , getInductVar
    , getNodePcEnv
    , getNodePcEnvWithTag
    , getPc
    , getPcWithTag
    , interpretCheck
    , interpretGroup
    , interpretHyp
    , interpretHypImps
    , liftUntagged
    , runAsmRefineGraphSliceT
    , runGraphSliceT
    , runTagged
    , tryGetNodePcEnv
    , withoutSendSExpr
    ) where

import BV.Core.GraphSlice.New.Common
import BV.Core.GraphSlice.New.Flat
import BV.Core.GraphSlice.New.Flatten
import BV.Core.GraphSlice.New.Flatten.PcEnv
import BV.Core.GraphSlice.New.Flatten.Tagged
import BV.Core.GraphSlice.New.SendFlatExprCommand
import BV.Core.GraphSlice.New.SendSolverExprCommand

import BV.Core.Logic (eqHandlingRelWrapper, strengthenHyp)
import BV.Core.Types
import BV.Core.Types.Extras
import BV.SMTLIB2 (SExpr)
import BV.Utils (ensureM)

import Control.Monad ((>=>))
import Control.Monad.Trans (lift)
import Control.Monad.Writer (Writer, execWriter, tell)
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

interpretGroup
    :: (RefineTag t, MonadGraphSliceSendSExpr m)
    => ProofCheckGroup t a
    -> GraphSliceT t m [SMTProofCheckImp a]
interpretGroup = traverse interpretCheck

interpretCheck
    :: (RefineTag t, MonadGraphSliceSendSExpr m)
    => ProofCheck t a
    -> GraphSliceT t m (SMTProofCheckImp a)
interpretCheck check = SMTProofCheckImp check.meta <$> do
    interpretHyp check.hyp
        >>= interpretHypImps check.hyps
        >>= convertExpr

interpretHypImps :: (RefineTag t, MonadGraphSliceSendSExpr m) => [Hyp t] -> FlatExpr -> GraphSliceT t m FlatExpr
interpretHypImps hyps concl = do
    hyps' <- traverse interpretHyp hyps
    return $ strengthenHyp $ nImpliesE hyps' concl

interpretHyp :: (RefineTag t, MonadGraphSliceSendSExpr m) => Hyp t -> GraphSliceT t m FlatExpr
interpretHyp = \case
    HypPcImp hyp -> do
        let f = \case
                PcImpHypSideBool v -> return $ fromBoolE v
                PcImpHypSidePc vt -> getPcWithTag vt
        impliesE <$> f hyp.lhs <*> f hyp.rhs
    HypEq { ifAt, eq } -> do
        extEnv <- case eq.induct of
            Just induct -> M.insert (Ident "%n") <$> getInductVar induct
            Nothing -> return id
        xPcEnvOpt <- getNodePcEnvWithTag eq.lhs.visit
        yPcEnvOpt <- getNodePcEnvWithTag eq.rhs.visit
        case (xPcEnvOpt, yPcEnvOpt) of
            (Just xPcEnv, Just yPcEnv) -> do
                let eq' = eqHandlingRelWrapper
                        (flattenExpr (extEnv xPcEnv.env) eq.lhs.expr)
                        (flattenExpr (extEnv yPcEnv.env) eq.rhs.expr)
                if ifAt
                    then do
                        xPc <- getPcWithTag eq.lhs.visit
                        yPc <- getPcWithTag eq.rhs.visit
                        return $ nImpliesE [xPc, yPc] eq'
                    else do
                        return eq'
            _ -> return $ fromBoolE ifAt

--

newtype DontSendSExpr a
  = DontSendSExprT { run :: Writer [SExprWithPlaceholders] a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance MonadGraphSliceSendSExpr DontSendSExpr where
    sendSExpr s = DontSendSExprT $ tell [s]

withoutSendSExpr :: Monad m => GraphSliceT t DontSendSExpr a -> GraphSliceT t m a
withoutSendSExpr = mapBase f g
  where
    f m = error $
        "withoutSendSExpr:\n"
            ++ concat [ showSExprWithPlaceholders s ++ "\n" | s <- execWriter m.run ]
    g _ = error "withoutSendSExpr"

--

class MonadGraphSliceSendSExpr m => MonadGraphSliceGetSExprValue m where
    getSExprValue :: SExprWithPlaceholders -> m SExpr

getFlatExprValue :: (Tag t, MonadGraphSliceGetSExprValue m) => FlatExpr -> GraphSliceT t m GraphExpr
getFlatExprValue expr = do
    sexpr <- withoutSendSExpr $ convertExpr expr
    valueSExpr <- lift $ getSExprValue sexpr
    let valueExpr = sexprToExpr valueSExpr
    ensureM $ valueExpr.ty == expr.ty
    return valueExpr
