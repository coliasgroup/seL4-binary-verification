module BV.Core.RepGraph.Old.AsmStackRep
    ( WithAsmStackRep
    , runWithAsmStackRep
    ) where

import BV.Core.RepGraph.Old.Core
import BV.Core.RepGraph.Old.Solver
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

instance MonadRepGraph AsmRefineTag m => MonadRepGraphDefaultHelper AsmRefineTag m (WithAsmStackRep m)

instance MonadRepGraph AsmRefineTag m => MonadRepGraph AsmRefineTag (WithAsmStackRep m) where
    runProblemVarRepHook kind var = runMaybeT $ do
        argRenames <- lift $ WithAsmStackRep ask
        let quadrant = PairingEqSideQuadrant Asm PairingEqDirectionIn
        let spName = argRenames quadrant (Ident "r13")
        guard $ var.tag == Asm
        guard $ var.value.ty == ExprTypeMem
        guard $ "stack" `isPrefixOf` var.value.name.unwrap
        guard $ kind /= VarRepRequestKindInit
        return $ VarRepRequestSplitMem
            { addr = varE word32T spName
            }
