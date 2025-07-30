{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Search.Inlining
    ( DiscoverInlineScriptInput (..)
    , discoverInlineScript
    ) where

import BV.Core.RepGraph
import BV.Core.Structs
import BV.Core.Types
import BV.Core.Types.Extras.Program (signatureOfFunction)
import BV.Search.Solver

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Map (Map)
import GHC.Generics (Generic)
import Optics

data DiscoverInlineScriptInput
  = DiscoverInlineScriptInput
      { structs :: ByTag' (Map Ident Struct)
      , rodata :: ROData
      , functions :: WithTag' Ident -> Function
      , pairingId :: PairingId'
      }
  deriving (Generic)

discoverInlineScript
    :: MonadRepGraphSolverInteract m
    => DiscoverInlineScriptInput
    -> m InlineScript'
discoverInlineScript input = buildProblemWith inliner lookupFun funs
  where
    lookupFun = input.functions
    funs = withTags input.pairingId <&> \wt -> Named wt.value (lookupFun wt)
    inliner problem = fmap (:[]) <$> nextReachableUnmatchedCInlinePoint (RepGraphBaseInput
        { structs = input.structs
        , rodata = input.rodata
        , problem
        , functionSigs = signatureOfFunction <$> lookupFun
        })

newtype InlineM m a
  = InlineM { run :: RepGraphBase AsmRefineTag (ExceptT InliningEvent (ReaderT InlinerInput m)) a }
  deriving (Functor)
  deriving newtype
    ( Applicative
    , Monad
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
  deriving (Generic)

instance MonadRepGraphSolverInteract m => MonadRepGraphDefaultHelper AsmRefineTag (RepGraphBase AsmRefineTag (ExceptT InliningEvent (ReaderT InlinerInput m))) (InlineM m) where
    liftMonadRepGraphDefaultHelper = InlineM

instance MonadRepGraphSolverInteract m => MonadRepGraph AsmRefineTag (InlineM m) where
    runPreEmitCallNodeHook _nodeId _pc _env = do
        undefined

runInlineM :: MonadRepGraphSolverInteract m => RepGraphBaseInput AsmRefineTag -> InlinerInput -> InlineM m a -> m (Either InliningEvent a)
runInlineM repGraphInput inlinerInput m =
    runReaderT (runExceptT (runRepGraphBase repGraphInput m.run)) inlinerInput

nextReachableUnmatchedCInlinePoint :: MonadRepGraphSolverInteract m => RepGraphBaseInput AsmRefineTag -> m (Maybe NodeAddr)
nextReachableUnmatchedCInlinePoint repGraphInput = preview (_Left % #nodeAddr) <$> ret
  where
    inlinerInput = InlinerInput
    ret = runInlineM repGraphInput inlinerInput $ do
        nextReachableUnmatchedCInlinePointInner

nextReachableUnmatchedCInlinePointInner :: MonadRepGraphSolverInteract m => InlineM m (Maybe NodeAddr)
nextReachableUnmatchedCInlinePointInner = undefined

--

-- TODO move

type Inliner m = Problem' -> m (Maybe [NodeAddr])

buildProblemWith :: Monad m => Inliner m -> (WithTag' Ident -> Function) -> ByTag' (Named Function) -> m InlineScript'
buildProblemWith _inliners _lookupFun _funs = undefined
