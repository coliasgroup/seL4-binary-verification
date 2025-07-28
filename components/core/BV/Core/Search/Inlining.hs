{-# LANGUAGE TypeFamilies #-}

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
import Data.Map (Map)
import GHC.Generics (Generic)
import Optics

data DiscoverInlineScriptInput
  = DiscoverInlineScriptInput
      { cStructs :: Map Ident Struct
      , functions :: WithTag Ident -> Function
      , pairings :: Pairings
      , rodata :: ROData
      , fnames :: PairingOf Ident
      }
  deriving (Generic)

discoverInlineScript
    :: (Monad m, S.MonadSolver n)
    => ((ModelConfig -> n a) -> m a)
    -> DiscoverInlineScriptInput
    -> m InlineScript
discoverInlineScript = undefined

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

instance S.MonadSolver m => MonadRepGraphDefaultHelper (M (ExceptT InliningEvent (ReaderT InlinerInput (InnerSolver m)))) (InlineM m) where
    liftMonadRepGraphDefaultHelper = InlineM

instance S.MonadSolver m => MonadRepGraph (InlineM m) where
    runPreEmitCallNodeHook _nodeId _pc _env = do
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

--

-- TODO move

type Inliner m = Problem -> m (Maybe [NodeAddr])

buildProblemWith :: Monad m => [Inliner m] -> (WithTag Ident -> Function) -> PairingOf (Named Function) -> InlineScript
buildProblemWith _inliners _lookupFun _funs = undefined
