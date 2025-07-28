{-# LANGUAGE MultiWayIf #-}

module BV.Core.Stages.CompileProofChecks.RepGraph.AsmStackRep
    ( WithAsmStackRep
    , runWithAsmStackRep
    ) where

import BV.Core.Logic
import BV.Core.Stages.CompileProofChecks.RepGraph
import BV.Core.Stages.CompileProofChecks.Solver
import BV.Core.Types
import BV.Core.Types.Extras

import Control.Monad (guard)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.List (isPrefixOf)

newtype WithAsmStackRep m a
  = WithAsmStackRep { run :: ReaderT ArgRenames m a }
  deriving (Functor)
  deriving newtype (Applicative, Monad, MonadTrans)

runWithAsmStackRep :: MonadRepGraph m => ArgRenames -> WithAsmStackRep m a -> m a
runWithAsmStackRep argRenames m = runReaderT m.run argRenames

instance MonadSolverSend m => MonadSolverSend (WithAsmStackRep m) where
    sendSExprWithPlaceholders = WithAsmStackRep . sendSExprWithPlaceholders

instance MonadStructs m => MonadStructs (WithAsmStackRep m) where
    askLookupStruct = WithAsmStackRep askLookupStruct

instance MonadSolver m => MonadSolver (WithAsmStackRep m) where
    liftSolver = WithAsmStackRep . liftSolver

instance MonadRepGraph m => MonadRepGraphDefaultHelper m (WithAsmStackRep m) where

instance MonadRepGraph m => MonadRepGraph (WithAsmStackRep m) where
    runProblemVarRepHook = asmStackRepHook

asmStackRepHook :: MonadRepGraph m => Ident -> ExprType -> VarRepRequestKind -> NodeAddr -> WithAsmStackRep m (Maybe Expr)
asmStackRepHook name ty kind n = runMaybeT $ do
    tag <- askNodeTag n
    guard $ tag == Asm
    guard $ "stack" `isPrefixOf` name.unwrap
    guard $ ty == ExprTypeMem
    guard $ kind /= VarRepRequestKindInit
    argRenames <- lift $ WithAsmStackRep ask
    return $ varE word32T $ argRenames
        (PairingEqSideQuadrant
            { tag
            , direction = PairingEqDirectionIn
            })
        (Ident "r13")
