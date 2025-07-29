{-# LANGUAGE MultiWayIf #-}

module BV.Core.Stages.RepGraph.AsmStackRep
    ( WithAsmStackRep
    , runWithAsmStackRep
    ) where

import BV.Core.Logic
import BV.Core.Stages.RepGraph.Core
import BV.Core.Stages.RepGraph.Solver
import BV.Core.Structs
import BV.Core.Types
import BV.Core.Types.Extras

import Control.Monad (guard)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.List (isPrefixOf)

newtype WithAsmStackRep m a
  = WithAsmStackRep { run :: ReaderT (ArgRenames AsmRefineTag) m a }
  deriving (Functor)
  deriving newtype (Applicative, Monad, MonadTrans)

runWithAsmStackRep :: MonadRepGraph AsmRefineTag m => ArgRenames AsmRefineTag -> WithAsmStackRep m a -> m a
runWithAsmStackRep argRenames m = runReaderT m.run argRenames

instance MonadRepGraphSolverSend m => MonadRepGraphSolverSend (WithAsmStackRep m) where
    sendSExprWithPlaceholders = WithAsmStackRep . sendSExprWithPlaceholders

instance MonadStructs m => MonadStructs (WithAsmStackRep m) where
    askLookupStruct = WithAsmStackRep askLookupStruct

instance MonadRepGraphSolver m => MonadRepGraphSolver (WithAsmStackRep m) where
    liftSolver = WithAsmStackRep . liftSolver

instance MonadRepGraph AsmRefineTag m => MonadRepGraphDefaultHelper AsmRefineTag m (WithAsmStackRep m) where

instance MonadRepGraph AsmRefineTag m => MonadRepGraph AsmRefineTag (WithAsmStackRep m) where
    runProblemVarRepHook = asmStackRepHook

asmStackRepHook :: MonadRepGraph AsmRefineTag m => NameTy -> VarRepRequestKind -> NodeAddr -> WithAsmStackRep m (Maybe Expr)
asmStackRepHook var kind n = runMaybeT $ do
    tag <- askNodeTag n
    guard $ tag == Asm
    guard $ "stack" `isPrefixOf` var.name.unwrap
    guard $ var.ty == ExprTypeMem
    guard $ kind /= VarRepRequestKindInit
    argRenames <- lift $ WithAsmStackRep ask
    return $ varE word32T $ argRenames
        (PairingEqSideQuadrant
            { tag
            , direction = PairingEqDirectionIn
            })
        (Ident "r13")
