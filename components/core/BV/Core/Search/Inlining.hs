{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Search.Inlining
    ( DiscoverInlineScriptInput (..)
    , discoverInlineScript
    ) where

import BV.Core.Logic (MonadStructs (..))
import BV.Core.ModelConfig (ModelConfig, configureSExpr)
import BV.Core.Stages.CompileProofChecks.RepGraph
import BV.Core.Stages.CompileProofChecks.Solver
import BV.Core.Types
import qualified BV.SMTLIB2.Monad as S

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Control.Monad.Trans (lift)
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
    :: (Applicative f, S.MonadSolver n)
    => ((ModelConfig -> n a) -> f a)
    -> DiscoverInlineScriptInput
    -> f InlineScript'
discoverInlineScript = undefined

newtype InlineM m a
  = InlineM { run :: RepGraphBase AsmRefineTag (ExceptT InliningEvent (ReaderT InlinerInput (InnerSolver m))) a }
  deriving (Functor)
  deriving newtype
    ( Applicative
    , Monad
    , MonadSolver
    , MonadSolverSend
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

newtype InnerSolver m a
  = InnerSolver { run :: ReaderT ModelConfig m a }
  deriving (Functor)
  deriving newtype (Applicative, Monad)

instance S.MonadSolver m => MonadSolverSend (InnerSolver m) where
    sendSExprWithPlaceholders s = InnerSolver $ do
        modelConfig <- ask
        lift $ S.sendSExpr $ configureSExpr modelConfig s

instance S.MonadSolver m => MonadRepGraphDefaultHelper AsmRefineTag (RepGraphBase AsmRefineTag (ExceptT InliningEvent (ReaderT InlinerInput (InnerSolver m)))) (InlineM m) where
    liftMonadRepGraphDefaultHelper = InlineM

instance S.MonadSolver m => MonadRepGraph AsmRefineTag (InlineM m) where
    runPreEmitCallNodeHook _nodeId _pc _env = do
        undefined

runInlineM :: S.MonadSolver m => ModelConfig -> RepGraphBaseInput AsmRefineTag -> InlinerInput -> InlineM m a -> m (Either InliningEvent a)
runInlineM modelConfig repGraphInput inlinerInput m =
    runReaderT (runReaderT (runExceptT (runRepGraphBase repGraphInput m.run)) inlinerInput).run modelConfig

nextInlinePoint :: S.MonadSolver m => ModelConfig -> RepGraphBaseInput AsmRefineTag -> m (Maybe NodeAddr)
nextInlinePoint modelConfig repGraphInput = preview (_Left % #nodeAddr) <$> ret
  where
    inlinerInput = undefined
    ret = runInlineM modelConfig repGraphInput inlinerInput $ do
        undefined

--

-- TODO move

type Inliner m = Problem' -> m (Maybe [NodeAddr])

buildProblemWith :: Monad m => [Inliner m] -> (WithTag' Ident -> Function) -> ByTag' (Named Function) -> InlineScript'
buildProblemWith _inliners _lookupFun _funs = undefined
