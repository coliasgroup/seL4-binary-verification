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
    , castExpr
    , exprArgs
    , exprOp -- TODO don't export
    , exprOpArgs -- TODO don't export
    , nodeAddrOf
    , nodeConts
    , programFromFunctions
    , renameVars
    , renameVarsA
    , signatureOfFunction
    , trivialNode
    , varSubst
    , varSubstAll
    , varSubstAllM
    , walkExprs
    , walkExprsGM
    , walkExprsM
    ) where

import BV.Core.Types
import BV.Core.Types.Extras.Expr (varFromNameTyE)
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
    traverseTopLevelLevelExprs :: Traversal' a GraphExpr

instance TraverseTopLevelExprs Node where
    traverseTopLevelLevelExprs =
        (#_NodeBasic % #varUpdates % traversed % traverseTopLevelLevelExprs)
            `adjoin` (#_NodeCond % #expr)
            `adjoin` (#_NodeCall % #input % traversed)

instance TraverseTopLevelExprs VarUpdate where
    traverseTopLevelLevelExprs = castOptic #val

instance TraverseTopLevelExprs GraphExpr where
    traverseTopLevelLevelExprs = castOptic simple

class FoldExprs c a where
    foldExprs :: Fold a (Expr c)

instance FoldExprs GraphExprContext Node where
    foldExprs = traverseTopLevelLevelExprs % foldExprs

instance FoldExprs GraphExprContext VarUpdate where
    foldExprs = traverseTopLevelLevelExprs % foldExprs

instance FoldExprs c (Expr c) where
    foldExprs = simple `summing` (#value % #_ExprValueOp % _2 % folded % foldExprs)

-- TODO move to Expr.hs

walkExprsM :: Monad m => (Expr c -> m (Expr c)) -> Expr c -> m (Expr c)
walkExprsM f = traverseOf (exprArgs % traversed) (walkExprsM f) >=> f

walkExprs :: (Expr c -> Expr c) -> Expr c -> Expr c
walkExprs f = runIdentity . walkExprsM (Identity . f)

walkExprsGM :: forall k (c' :: k) (c :: k) m. Monad m => (forall (c'' :: k). Expr c'' -> m (Expr c)) -> Expr c' -> m (Expr c)
walkExprsGM f = traverseOf (exprArgs % traversed) (walkExprsGM f) >=> f

varSubstAllM :: Monad m => (NameTy -> m (Expr c')) -> Expr c -> m (Expr c')
varSubstAllM f expr = case expr.value of
    ExprValueVar name -> f (NameTy name expr.ty)
    _ -> traverseOf (exprArgs % traversed) (varSubstAllM f) expr

varSubstAll :: (NameTy -> (Expr c')) -> Expr c -> Expr c'
varSubstAll f = runIdentity . varSubstAllM (Identity . f)

varSubst :: (NameTy -> Maybe (Expr c)) -> Expr c -> Expr c
varSubst f = varSubstAll (\var -> fromMaybe (varFromNameTyE var) (f var))

--

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

instance HasVarNames (Expr c) where
    varNamesOf = #value % (#_ExprValueVar `adjoin` (#_ExprValueOp % _2 % traversed % varNamesOf))

instance HasVarNames (ExprValue c) where
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

-- TODO move to Expr.hs

castExpr :: Expr c -> Expr c'
castExpr = over (exprArgs % traversed) castExpr

exprOp :: AffineTraversal' (Expr c) Op
exprOp = #value % #_ExprValueOp % _1

exprOpArgs :: AffineTraversal (Expr c) (Expr c') (Op, [Expr c]) (Op, [Expr c'])
exprOpArgs = exprValue % exprValueOpArgs

exprValue :: Lens (Expr c) (Expr c') (ExprValue c) (ExprValue c')
exprValue = lens (view #value) (\(Expr ty _) -> Expr ty)

exprArgs :: AffineTraversal (Expr c) (Expr c') [Expr c] [Expr c']
exprArgs = exprValue % exprValueArgs

exprValueArgs :: AffineTraversal (ExprValue c) (ExprValue c') [Expr c] [Expr c']
exprValueArgs = exprValueOpArgs % _2

exprValueOpArgs :: Prism (ExprValue c) (ExprValue c') (Op, [Expr c]) (Op, [Expr c'])
exprValueOpArgs = prism (uncurry ExprValueOp) (\case
    ExprValueOp op args -> Right (op, args)
    ExprValueVar x -> Left (ExprValueVar x)
    ExprValueNum x -> Left (ExprValueNum x)
    ExprValueType x -> Left (ExprValueType x)
    ExprValueSymbol x -> Left (ExprValueSymbol x)
    ExprValueToken x -> Left (ExprValueToken x)
    ExprValueSMTExpr x -> Left (ExprValueSMTExpr x))

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
