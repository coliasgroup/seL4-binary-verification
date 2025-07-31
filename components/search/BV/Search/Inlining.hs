{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Search.Inlining
    ( DiscoverInlineScriptInput (..)
    , discoverInlineScript
    ) where

import BV.Core.Graph
import BV.Core.RepGraph
import BV.Core.Stages
import BV.Core.Structs
import BV.Core.Types
import BV.Core.Types.Extras.Expr
import BV.Core.Types.Extras.Program
import BV.Core.Types.Extras.ProofCheck
import BV.Search.Solver
import BV.Search.Utils

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError,
                             tryError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (lift)
import Data.Foldable (for_)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

data DiscoverInlineScriptInput
  = DiscoverInlineScriptInput
      { structs :: ByTag' (Map Ident Struct)
      , rodata :: ROData
      , functions :: WithTag' Ident -> Function
      , pairings :: S.Set PairingId'
      , pairingId :: PairingId'
      }
  deriving (Generic)

discoverInlineScript
    :: MonadRepGraphSolverInteract m
    => DiscoverInlineScriptInput
    -> m InlineScript'
discoverInlineScript input = evalStateT (buildInlineScript composeInliners lookupFun funs) inliners
  where
    lookupFun = input.functions
    funs = withTags input.pairingId <&> \wt -> Named wt.value (lookupFun wt)
    matched = S.fromList $ input.pairings ^.. folded % folded
    matchedC = S.fromList $ input.pairings ^.. folded % atTag Asm
    inliners = [inlineCompletelyUnmatched, inlineReachableUnmatchedC]
    inlineCompletelyUnmatched = return . nextCompletelyUnmatchedInlinePoints matched
    inlineReachableUnmatchedC problem = fmap (:[]) <$> nextReachableUnmatchedCInlinePoint matchedC (RepGraphBaseInput
        { structs = input.structs
        , rodata = input.rodata
        , problem
        })

composeInliners :: Monad m => Inliner t (StateT [Inliner t m] m)
composeInliners problem = go
  where
    go = get >>= \case
        [] -> return Nothing
        x:xs -> lift (x problem) >>= \case
            Nothing -> put xs >> go
            Just y -> return (Just y)

nextCompletelyUnmatchedInlinePoints :: S.Set Ident -> Problem' -> Maybe [NodeAddr]
nextCompletelyUnmatchedInlinePoints matched p = case M.keys (M.filter f p.nodes) of
    [] -> Nothing
    addrs -> Just addrs
  where
    f = \case
        NodeCall callNode -> S.notMember callNode.functionName matched
        _ -> False

newtype InlineM m a
  = InlineM { run :: RepGraphBase AsmRefineTag (ExceptT InliningEvent (ReaderT InlinerInput m)) a }
  deriving (Functor)
  deriving newtype
    ( Applicative
    , Monad
    , MonadError InliningEvent
    , MonadRepGraphSolver
    , MonadRepGraphSolverSend
    , MonadStructs
    )

data InliningEvent
  = InliningEvent
      { nodeAddr :: NodeAddr
      }
  deriving (Generic)

data InlinerInput
  = InlinerInput
      { matchedC :: S.Set Ident
      }
  deriving (Generic)

instance MonadRepGraphSolverInteract m => MonadRepGraphDefaultHelper AsmRefineTag (RepGraphBase AsmRefineTag (ExceptT InliningEvent (ReaderT InlinerInput m))) (InlineM m) where
    liftMonadRepGraphDefaultHelper = InlineM

instance MonadRepGraphSolverInteract m => MonadRepGraph AsmRefineTag (InlineM m) where
    runPreEmitCallNodeHook visit pc env = do
        let nodeAddr = nodeAddrFromNodeId visit.nodeId
        p <- askProblem
        tag <- askNodeTag nodeAddr
        let fname = p ^. #nodes % at nodeAddr % unwrapped % expecting #_NodeCall % #functionName
        matchedC <- InlineM $ lift $ lift $ gview #matchedC
        when (tag == C && S.notMember fname matchedC) $ do
            hyp <- withEnv env $ convertExprNoSplit $ notE pc
            res <- InlineM $ lift $ lift $ lift $ checkHyp hyp -- TODO get rid of lifts
            unless res $ do
                throwError $ InliningEvent
                    { nodeAddr
                    }

runInlineM :: MonadRepGraphSolverInteract m => RepGraphBaseInput AsmRefineTag -> InlinerInput -> InlineM m a -> m (Either InliningEvent a)
runInlineM repGraphInput inlinerInput m =
    runReaderT (runExceptT (runRepGraphBase repGraphInput m.run)) inlinerInput

nextReachableUnmatchedCInlinePoint :: MonadRepGraphSolverInteract m => S.Set Ident -> RepGraphBaseInput AsmRefineTag -> m (Maybe NodeAddr)
nextReachableUnmatchedCInlinePoint matchedC repGraphInput = preview (_Left % #nodeAddr) <$> ret
  where
    inlinerInput = InlinerInput
        { matchedC
        }
    ret = runInlineM repGraphInput inlinerInput nextReachableUnmatchedCInlinePointInner

nextReachableUnmatchedCInlinePointInner :: MonadRepGraphSolverInteract m => InlineM m ()
nextReachableUnmatchedCInlinePointInner = do
    p <- askProblem
    loopDataMap <- askLoopDataMap
    let heads = loopHeadsIncludingInner p.nodes loopDataMap
    let limits = [ Restr n (doubleRangeVC 3 3) | n <- heads ]
    for_ (M.keys p.nodes) $ \n -> tryError $ getNodePcEnv (Visit (Addr n) limits) Nothing
    getNodePcEnv (Visit Ret limits) (Just C)
    getNodePcEnv (Visit Err limits) (Just C)
    return ()
