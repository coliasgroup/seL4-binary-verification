{-# LANGUAGE RecordWildCards #-}

module BV.Core.Stages.CompileProofChecks.RepGraph.Concrete
    ( M
    , RepGraphInput (..)
    , runM
    ) where

import BV.Core.Logic
import BV.Core.Stages.CompileProofChecks.RepGraph
import BV.Core.Stages.CompileProofChecks.Solver
import BV.Core.Stages.CompileProofChecks.Structs
import BV.Core.Types

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT, mapReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT, mapStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Map (Map)
import GHC.Generics (Generic)
import Optics

data RepGraphInput t
  = RepGraphInput
      { cStructs :: Map Ident Struct
      , rodata :: ROData
      , problem :: Problem t
      , functionSigs :: FunctionSignatures t
      }
  deriving (Generic)

newtype M t m a
  = M { run :: StateT (State t) (ReaderT (Env t) m) a }
  deriving (Functor)
  deriving newtype (Applicative, Monad)

runM :: (Tag t, MonadSolverSend m) => RepGraphInput t -> M t m a -> m a
runM input m = runReaderT (evalStateT m'.run initState) env
  where
    env = initEnv input
    m' = do
        initSolver
        initRepGraph
        m

instance MonadTrans (M t) where
    lift = M . lift . lift

instance MonadSolverSend m => MonadSolverSend (M t m) where
    sendSExprWithPlaceholders = M . sendSExprWithPlaceholders

data Env t
  = Env
      { structs :: Ident -> Struct
      , solver :: SolverEnv
      , repGraph :: RepGraphEnv t
      }
  deriving (Generic)

data State t
  = State
      { solver :: SolverState
      , repGraph :: RepGraphState t
      }
  deriving (Generic)

instance Monad m => MonadStructs (M t m) where
    askLookupStruct = M $ gview #structs

instance MonadSolverSend m => MonadSolver (M t m) where
    liftSolver m = M
        . zoom #solver
        . magnify #solver
        . mapStateT (mapReaderT (return . runIdentity))
        $ m

instance (Tag t, MonadSolverSend m) => MonadRepGraph t (M t m) where
    liftRepGraph m = M
        . zoom #repGraph
        . magnify #repGraph
        . mapStateT (mapReaderT (return . runIdentity))
        $ m

    runProblemVarRepHook _ _ _ _ = return Nothing
    runPostEmitNodeHook _ = return ()
    runPreEmitCallNodeHook _ _ _ = return ()
    runPostEmitCallNodeHook _ _ _ _ = return ()

initEnv :: Tag t => RepGraphInput t -> Env t
initEnv (RepGraphInput {..}) = Env
    { structs = initStructsEnv rodata problem cStructs
    , solver = initSolverEnv rodata
    , repGraph = initRepGraphEnv problem functionSigs
    }

initState :: State t
initState = State
    { solver = initSolverState
    , repGraph = initRepGraphState
    }
