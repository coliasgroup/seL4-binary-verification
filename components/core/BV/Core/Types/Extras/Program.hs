{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.Extras.Program
    ( FoldExprs (..)
    , FunctionFilter
    , FunctionSignature (..)
    , HasVarDecls (..)
    , HasVarNames (..)
    , LookupFunctionSignature
    , TraverseTopLevelExprs (..)
    , VarUpdate (..)
    , applyFunctionFilter
    , exprOp
    , exprOpArgs
    , nodeAddrOf
    , nodeConts
    , programFromFunctions
    , renameVars
    , renameVarsA
    , signatureOfFunction
    , trivialNode
    , varSubst
    , walkExprs
    , walkExprsM
    ) where

import BV.Core.Types
import BV.Core.Utils.IncludeExcludeFilter (IncludeExcludeFilter,
                                           applyIncludeExcludeFilter)
import BV.Utils (expecting)

import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Control.Monad.Identity (Identity (Identity, runIdentity))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Optics

--

programFromFunctions :: M.Map Ident Function -> Program
programFromFunctions functions = mempty & #functions .~ functions

trivialNode :: NodeId -> Node
trivialNode next = NodeBasic (BasicNode { next, varUpdates = [] })

nodeAddrOf :: NodeId -> NodeAddr
nodeAddrOf = view $ expecting #_Addr

type FunctionFilter = IncludeExcludeFilter Ident

applyFunctionFilter :: FunctionFilter -> Program -> Program
applyFunctionFilter f =
    #functions %~ M.filterWithKey (\k _v -> applyIncludeExcludeFilter f k)

--

class TraverseTopLevelExprs a where
    traverseTopLevelLevelExprs :: Traversal' a Expr

instance TraverseTopLevelExprs Node where
    traverseTopLevelLevelExprs =
        (#_NodeBasic % #varUpdates % traversed % traverseTopLevelLevelExprs)
            `adjoin` (#_NodeCond % #expr)
            `adjoin` (#_NodeCall % #input % traversed)

instance TraverseTopLevelExprs VarUpdate where
    traverseTopLevelLevelExprs = castOptic #val

instance TraverseTopLevelExprs Expr where
    traverseTopLevelLevelExprs = castOptic simple

class FoldExprs a where
    foldExprs :: Fold a Expr

instance FoldExprs Node where
    foldExprs = traverseTopLevelLevelExprs % foldExprs

instance FoldExprs VarUpdate where
    foldExprs = traverseTopLevelLevelExprs % foldExprs

instance FoldExprs Expr where
    foldExprs = simple `summing` (#value % #_ExprValueOp % _2 % folded % foldExprs)

walkExprsM :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
walkExprsM f = traverseOf #value recurse >=> f
  where
    recurse = \case
        ExprValueOp op args -> ExprValueOp op <$> traverse (walkExprsM f) args
        v -> return v

walkExprs :: (Expr -> Expr) -> Expr -> Expr
walkExprs f = runIdentity . walkExprsM (Identity . f)

varSubst :: (NameTy -> Maybe Expr) -> Expr -> Expr
varSubst f = walkExprs $ \case
        expr@(Expr ty (ExprValueVar ident)) -> fromMaybe expr $ f $ NameTy ident ty
        expr -> expr

class HasVarNames a where
    varNamesOf :: Traversal' a Ident

instance HasVarNames NameTy where
    varNamesOf = castOptic #name

instance HasVarNames Node where
    varNamesOf =
        (#_NodeBasic % #varUpdates % traversed % varNamesOf)
            `adjoin` (#_NodeCond % #expr % varNamesOf)
            `adjoin` (#_NodeCall % adjoin (#input % traversed % varNamesOf) (#output % traversed % varNamesOf))

instance HasVarNames VarUpdate where
    varNamesOf = (#var % #name) `adjoin` #val % varNamesOf

instance HasVarNames Expr where
    varNamesOf = #value % (#_ExprValueVar `adjoin` (#_ExprValueOp % _2 % traversed % varNamesOf))

instance HasVarNames ExprValue where
    varNamesOf = castOptic #_ExprValueVar

renameVarsA :: (HasVarNames a, Applicative f) => (Ident -> f Ident) -> a -> f a
renameVarsA = traverseOf varNamesOf

renameVars :: HasVarNames a => (Ident -> Ident) -> a -> a
renameVars f = runIdentity . renameVarsA (Identity . f)

class HasVarDecls a where
    varDeclsOf :: Traversal' a NameTy

instance HasVarDecls Function where
    varDeclsOf =
        ((#input `adjoin` #output) % traversed % varDeclsOf)
        `adjoin`
        (#body % traversed % varDeclsOf)

instance HasVarDecls FunctionBody where
    varDeclsOf = #nodes % varDeclsOf

instance HasVarDecls NameTy where
    varDeclsOf = castOptic simple

instance HasVarDecls NodeMap where
    varDeclsOf = traversed % varDeclsOf

instance HasVarDecls Node where
    varDeclsOf = adjoin
        (#_NodeBasic % #varUpdates % traversed % varDeclsOf)
        (#_NodeCall % #output % traversed % varDeclsOf)

instance HasVarDecls VarUpdate where
    varDeclsOf = castOptic #var

nodeConts :: Traversal' Node NodeId
nodeConts = castOptic $
    (#_NodeBasic % #next)
        `adjoin`(#_NodeCond % (#left `adjoin` #right))
        `adjoin` (#_NodeCall % #next)

--

exprOp :: Lens' Expr Op
exprOp = #value % expecting #_ExprValueOp % _1

exprOpArgs :: Lens' Expr [Expr]
exprOpArgs = #value % expecting #_ExprValueOp % _2

--

data FunctionSignature
  = FunctionSignature
      { input :: [NameTy]
      , output :: [NameTy]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

signatureOfFunction :: Function -> FunctionSignature
signatureOfFunction fun = FunctionSignature
    { input = fun.input
    , output = fun.output
    }

type LookupFunctionSignature t = WithTag t Ident -> FunctionSignature
