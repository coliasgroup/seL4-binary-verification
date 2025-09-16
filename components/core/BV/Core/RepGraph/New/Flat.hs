module BV.Core.RepGraph.New.Flat
    ( NameHint
    , RepGraphFlatT
    , addDef
    , addVar
    , assertFlatExpr
    , cacheExpr
    , cacheExprInline
    , lookupDef
    , runRepGraphFlatTStep
    ) where

import BV.Core.RepGraph.New.Common
import BV.Core.RepGraph.New.SendFlatExprCommand

import BV.Core.GenerateFreshName (takeFreshNameWith)
import BV.Core.Types
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

type T = RepGraphFlatT

type InnerT = RepGraphSendFlatExprCommandT

type C = MonadRepGraphSendSExpr

--

newtype RepGraphFlatT m a
  = RepGraphFlatT { run :: StateT TState (InnerT m) a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance Monad m => MonadInner (InnerT m) (T m) where
    liftInner = RepGraphFlatT . lift

instance MonadTrans T where
    lift = liftInner . lift

liftPure :: Monad m => State TState a -> T m a
liftPure = RepGraphFlatT . mapStateT (return . runIdentity)

send :: C m => FlatExprCommand -> T m ()
send = liftInner . sendFlatExprCommand

type NameHint = String

data TState
  = TState
      { names :: Set Ident
      , defs :: Map Ident FlatExpr
      , exprCache :: Map FlatExpr Ident
      }
  deriving (Generic)

runRepGraphFlatTStep :: Monad m => T m a -> InnerT m a
runRepGraphFlatTStep = flip evalStateT initState . (.run)

initState :: TState
initState = TState
    { names = S.empty
    , defs = M.empty
    , exprCache = M.empty
    }

addVar :: C m => NameHint -> ExprType -> T m NameTy
addVar nameHint ty = do
    name <- takeFreshName nameHint
    let var = NameTy name ty
    send $ ExprCommandDeclare var
    return var

addDef :: C m => NameHint -> FlatExpr -> T m NameTy
addDef = addDefWithInlineHint ExprCommandInlineHintNever

addDefWithInlineHint :: C m => ExprCommandInlineHint -> NameHint -> FlatExpr -> T m NameTy
addDefWithInlineHint inline nameHint expr = do
    name <- takeFreshName nameHint
    let var = NameTy name expr.ty
    send $ ExprCommandDefine inline var expr
    liftPure $ #defs %= M.insert name expr
    return var

cacheExpr :: C m => NameHint -> FlatExpr -> T m NameTy
cacheExpr = cacheExprWithInlineHint ExprCommandInlineHintNever

cacheExprInline :: C m => NameHint -> FlatExpr -> T m NameTy
cacheExprInline = cacheExprWithInlineHint ExprCommandInlineHintSometimes

cacheExprWithInlineHint :: C m => ExprCommandInlineHint -> NameHint -> FlatExpr -> T m NameTy
cacheExprWithInlineHint inline nameHint expr = flip NameTy expr.ty <$> case expr.value of
    ExprValueVar name -> return name
    _ -> withMapSlotWith liftPure #exprCache expr $ (.name)
            <$> addDefWithInlineHint inline nameHint expr

lookupDef :: C m =>  Ident -> T m (Maybe FlatExpr)
lookupDef name = liftPure $ use $ #defs % at name

takeFreshName :: C m => NameHint -> T m Ident
takeFreshName nameHint = liftPure $ zoom #names $ takeFreshNameWith Ident nameHint

assertFlatExpr :: C m => FlatExpr -> T m ()
assertFlatExpr = send . ExprCommandAssert
