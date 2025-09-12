{-# LANGUAGE UndecidableInstances #-}

module BV.Core.Experimental.RepGraph.Base
    ( RepGraphBase
    , RepGraphBaseInput (..)
    , runRepGraphBase
    ) where

import BV.Core.Experimental.RepGraph.Core
import BV.Core.Experimental.RepGraph.Flatten
import BV.Core.Experimental.RepGraph.Solver

import BV.Core.Structs
import BV.Core.Types

import BV.Core.Types.Extras (machineWordE, pointerE, structT)
import Control.Monad.Except (MonadError)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (..), ReaderT, mapReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, mapStateT, state)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer (MonadWriter)
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
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad, MonadError e, MonadWriter w)

data Env t
  = Env
      { structs :: Ident -> Struct
      , solver :: SolverEnv
      , flatten :: FlattenEnv
      , repGraph :: RepGraphEnv t
      }
  deriving (Generic)

data State t
  = State
      { solver :: SolverState
      , flatten :: FlattenState
      , repGraph :: RepGraphState t
      }
  deriving (Generic)

instance MonadTrans (RepGraphBase t) where
    lift = RepGraphBase . lift . lift

instance MonadReader r m => MonadReader r (RepGraphBase t m) where
    ask = lift ask
    local f = #run %~ (mapStateT . mapReaderT) (local f)

instance MonadState s m => MonadState s (RepGraphBase t m) where
    state = lift . state

instance MonadRepGraphSolverSend m => MonadRepGraphSolverSend (RepGraphBase t m) where
    sendSExprWithPlaceholders = lift . sendSExprWithPlaceholders

instance MonadRepGraphSolverSend m => MonadRepGraphFlattenSend (RepGraphBase t m) where
    sendCommand = convertCommand

-- instance MonadRepGraphFlattenSend m => MonadRepGraphFlattenSend (RepGraphBase t m) where
--     sendCommand = lift . sendCommand

instance Monad m => MonadStructs (RepGraphBase t m) where
    askLookupStruct = RepGraphBase $ gview #structs

instance MonadRepGraphSolverSend m => MonadRepGraphSolver (RepGraphBase t m) where
    liftSolver m = RepGraphBase
        . zoom #solver
        . magnify #solver
        . mapStateT (mapReaderT (return . runIdentity))
        $ m

instance MonadRepGraphSolverSend m => MonadRepGraphFlatten (RepGraphBase t m) where
    liftFlatten m = RepGraphBase
        . zoom #flatten
        . magnify #flatten
        . mapStateT (mapReaderT (return . runIdentity))
        $ m

instance (Tag t, MonadRepGraphSolverSend m) => MonadRepGraph t (RepGraphBase t m) where
    liftRepGraph m = RepGraphBase
        . zoom #repGraph
        . magnify #repGraph
        . mapStateT (mapReaderT (return . runIdentity))
        $ m

    runProblemVarRepHook _ _ _ = return Nothing
    runPreEmitCallNodeHook _ _ = return ()
    runPostEmitCallNodeHook _ = return ()

runRepGraphBase :: (Tag t, MonadRepGraphSolverSend m) => RepGraphBaseInput t -> RepGraphBase t m a -> m a
runRepGraphBase input m = runReaderT (evalStateT m'.run initState) env
  where
    env = initEnv input
    m' = do
        initSolver
        initRepGraph
        m

initEnv :: Tag t => RepGraphBaseInput t -> Env t
initEnv input = Env
    { structs
    , solver = initSolverEnv input.rodata
    , flatten = initFlattenEnv rodataPtrs
    , repGraph = initRepGraphEnv input.problem
    }
  where
    structs = (M.!) $ M.unionsWith (error "unexpected") $
        rodataStructsOf input.rodata : toList input.structs
    rodataPtrs =
        [ pointerE (structT structName) (machineWordE range.addr)
        | (structName, range) <- rodataStructNamesOf input.rodata
        ]

initState :: State t
initState = State
    { solver = initSolverState
    , flatten = initFlattenState
    , repGraph = initRepGraphState
    }
