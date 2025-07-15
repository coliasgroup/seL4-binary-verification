{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Search.Inlining
    (
    ) where

import BV.Core.Logic (MonadStructs (..))
import BV.Core.ModelConfig (ModelConfig, configureSExpr)
import BV.Core.Stages.CompileProofChecks.ConcreteRepGraph
import BV.Core.Stages.CompileProofChecks.RepGraph
import BV.Core.Stages.CompileProofChecks.Solver
import BV.Core.Types
import qualified BV.SMTLIB2.Monad as S

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import GHC.Generics (Generic)
import Optics

newtype InlineM m a
  = InlineM { run :: M (ExceptT InliningEvent (ReaderT InlinerInput (InnerSolver m))) a }
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

instance S.MonadSolver m => MonadRepGraph (InlineM m) where
    liftRepGraph = InlineM . liftRepGraph

    runPreEmitNodeHook _visit = do
        undefined

runInlineM :: S.MonadSolver m => ModelConfig -> RepGraphInput -> InlinerInput -> InlineM m a -> m (Either InliningEvent a)
runInlineM modelConfig repGraphInput inlinerInput m =
    runReaderT (runReaderT (runExceptT (runM repGraphInput m.run)) inlinerInput).run modelConfig

nextInlinePoint :: S.MonadSolver m => ModelConfig -> RepGraphInput -> m (Maybe NodeAddr)
nextInlinePoint modelConfig repGraphInput = preview (_Left % #nodeAddr) <$> ret
  where
    inlinerInput = undefined
    ret = runInlineM modelConfig repGraphInput inlinerInput $ do
        undefined

-- type Inliner m = Problem -> m (Maybe NodeAddr)

-- buildProblemWith :: Monad m => Inliner m -> (WithTag Ident -> Function) -> InlineScript -> PairingOf (Named Function) -> Problem
-- buildProblemWith lookupFun inlineScript funs = undefined
