module BV.Core.GraphSlice.New.Flat
    ( GraphSliceFlatT
    , NameHint
    , addDef
    , addVar
    , assertFlatExpr
    , cacheExpr
    , cacheExprInline
    , lookupDef
    , runGraphSliceFlatTStep
    ) where

import BV.Core.GraphSlice.New.Common
import BV.Core.GraphSlice.New.SendFlatExprCommand

import BV.Core.GenerateFreshName (takeFreshNameWith)
import BV.Core.Types
import BV.Core.Types.Extras (varFromNameTyE)
import BV.Core.Utils (withMapSlotWith)

import Control.Monad.Identity (runIdentity)
import Control.Monad.State (State, StateT, evalStateT, mapStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))

type T = GraphSliceFlatT

type InnerT = GraphSliceSendFlatExprCommandT

type C = MonadGraphSliceSendSExpr

--

newtype GraphSliceFlatT m a
  = GraphSliceFlatT { run :: StateT TState (InnerT m) a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance MonadTrans T where
    lift = liftInner . lift

instance MonadLiftInner InnerT T where
    liftInner = GraphSliceFlatT . lift

instance MonadMapInnermost T where
    mapInnermost f = #run %~ mapStateT (mapInnermost f)

liftPure :: Monad m => State TState a -> T m a
liftPure = GraphSliceFlatT . mapStateT (return . runIdentity)

send :: C m => FlatExprCommand -> T m ()
send = liftInner . sendFlatExprCommand

type NameHint = String

data TState
  = TState
      { names :: Set Ident
      , cache :: Map FlatExpr FlatExpr
      , defs :: Map Ident FlatExpr
      }
  deriving (Generic)

runGraphSliceFlatTStep :: Monad m => T m a -> InnerT m a
runGraphSliceFlatTStep = flip evalStateT initState . (.run)

initState :: TState
initState = TState
    { names = S.empty
    , cache = M.empty
    , defs = M.empty
    }

addVar :: C m => NameHint -> ExprType -> T m NameTy
addVar nameHint ty = do
    name <- takeFreshName nameHint
    let var = NameTy name ty
    send $ ExprCommandDeclare var
    return var

addDef :: C m => NameHint -> FlatExpr -> T m FlatExpr
addDef = addDefWithInlineHint ExprCommandInlineHintNever

addDefWithInlineHint :: C m => ExprCommandInlineHint -> NameHint -> FlatExpr -> T m FlatExpr
addDefWithInlineHint inline nameHint expr = do
    name <- takeFreshName nameHint
    let var = NameTy name expr.ty
    send $ ExprCommandDefine inline var expr
    liftPure $ #defs %= M.insert name expr
    return $ varFromNameTyE var

cacheExpr :: C m => NameHint -> FlatExpr -> T m FlatExpr
cacheExpr = cacheExprWithInlineHint ExprCommandInlineHintNever

cacheExprInline :: C m => NameHint -> FlatExpr -> T m FlatExpr
cacheExprInline = cacheExprWithInlineHint ExprCommandInlineHintSometimes

cacheExprWithInlineHint :: C m => ExprCommandInlineHint -> NameHint -> FlatExpr -> T m FlatExpr
cacheExprWithInlineHint inline nameHint expr = case expr.value of
    ExprValueVar _ -> return expr
    _ -> withMapSlotWith liftPure #cache expr $ addDefWithInlineHint inline nameHint expr

lookupDef :: C m =>  Ident -> T m (Maybe FlatExpr)
lookupDef name = liftPure $ use $ #defs % at name

takeFreshName :: C m => NameHint -> T m Ident
takeFreshName nameHint = liftPure $ zoom #names $ takeFreshNameWith Ident nameHint

assertFlatExpr :: C m => FlatExpr -> T m ()
assertFlatExpr = send . ExprCommandAssert
