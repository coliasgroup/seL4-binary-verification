{-# LANGUAGE RecordWildCards #-}

module BV.Core.RepGraph.Base
    ( RepGraphBase
    , RepGraphBaseInput (..)
    , runRepGraphBase
    ) where

import BV.Core.RepGraph.Core
import BV.Core.RepGraph.Solver
import BV.Core.Structs
import BV.Core.Types

import Control.Monad.Except (MonadError)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT, mapReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT, mapStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics

data RepGraphBaseInput t
  = RepGraphBaseInput
      { structs :: ByTag t (Map Ident Struct)
      , rodata :: ROData
      , problem :: Problem t
      }
  deriving (Generic)

newtype RepGraphBase t m a
  = RepGraphBase { run :: StateT (State t) (ReaderT (Env t) m) a }
  deriving (Functor)
  deriving newtype (Applicative, Monad, MonadError e)

runRepGraphBase :: (Tag t, MonadRepGraphSolverSend m) => RepGraphBaseInput t -> RepGraphBase t m a -> m a
runRepGraphBase input m = runReaderT (evalStateT m'.run initState) env
  where
    env = initEnv input
    m' = do
        initSolver
        initRepGraph
        m

instance MonadTrans (RepGraphBase t) where
    lift = RepGraphBase . lift . lift

instance MonadRepGraphSolverSend m => MonadRepGraphSolverSend (RepGraphBase t m) where
    sendSExprWithPlaceholders = RepGraphBase . sendSExprWithPlaceholders

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

instance Monad m => MonadStructs (RepGraphBase t m) where
    askLookupStruct = RepGraphBase $ gview #structs

instance MonadRepGraphSolverSend m => MonadRepGraphSolver (RepGraphBase t m) where
    liftSolver m = RepGraphBase
        . zoom #solver
        . magnify #solver
        . mapStateT (mapReaderT (return . runIdentity))
        $ m

instance (Tag t, MonadRepGraphSolverSend m) => MonadRepGraph t (RepGraphBase t m) where
    liftRepGraph m = RepGraphBase
        . zoom #repGraph
        . magnify #repGraph
        . mapStateT (mapReaderT (return . runIdentity))
        $ m

    runProblemVarRepHook _ _ _ = return Nothing
    runPostEmitNodeHook _ = return ()
    runPreEmitCallNodeHook _ _ _ = return ()
    runPostEmitCallNodeHook _ _ _ _ = return ()

initEnv :: Tag t => RepGraphBaseInput t -> Env t
initEnv input = Env
    { structs
    , solver = initSolverEnv input.rodata
    , repGraph = initRepGraphEnv input.problem
    }
  where
    structs = (M.!) $ M.unionsWith (error "unexpected") $
        rodataStructsOf input.rodata : toList input.structs

initState :: State t
initState = State
    { solver = initSolverState
    , repGraph = initRepGraphState
    }
