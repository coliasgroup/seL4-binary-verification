{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.Program
    ( Argument (..)
    , ConstGlobal (..)
    , Expr (..)
    , ExprType (..)
    , ExprValue (..)
    , FoldExprs (..)
    , Function (..)
    , FunctionBody (..)
    , HasVarBindings (..)
    , HasVarNames (..)
    , Ident (..)
    , Named (..)
    , Node (..)
    , NodeAddr (..)
    , NodeId (..)
    , NodeMap
    , Op (..)
    , Program (..)
    , Struct (..)
    , StructField (..)
    , TraverseTopLevelExprs (..)
    , VarUpdate (..)
    , fromListOfNamed
    , nodeConts
    , renameVars
    , toListOfNamed
    , walkExprs
    , walkExprsI
    ) where

import Control.DeepSeq (NFData)
import Control.Monad.Identity (Identity (Identity, runIdentity))
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics.Core

import BV.Core.Utils

newtype Ident
  = Ident { unwrap :: String }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

data Named a
  = Named
      { name :: Ident
      , value :: a
      }
  deriving (Eq, Generic, NFData, Ord, Show)

toListOfNamed :: Map Ident a -> [Named a]
toListOfNamed = map (\(name, value) -> Named { name, value }) . M.toList

fromListOfNamed :: [Named a] -> Map Ident a
fromListOfNamed = M.fromList . map (\Named { name, value } -> (name, value))

data Program
  = Program
      { structs :: Map Ident Struct
      , constGlobals :: Map Ident ConstGlobal
      , functions :: Map Ident Function
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data Struct
  = Struct
      { size :: Integer
      , align :: Integer
      , fields :: Map Ident StructField
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data StructField
  = StructField
      { ty :: ExprType
      , offset :: Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data ConstGlobal
  = ConstGlobal
      { value :: Expr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data Function
  = Function
      { input :: [Argument]
      , output :: [Argument]
      , body :: Maybe FunctionBody
      }
  deriving (Eq, Generic, NFData, Ord, Show)

type NodeMap = Map NodeAddr Node

data FunctionBody
  = FunctionBody
      { entryPoint :: NodeId
      , nodes :: NodeMap
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data Argument
  = Argument
      { name :: Ident
      , ty :: ExprType
      }
  deriving (Eq, Generic, NFData, Ord, Show)

newtype NodeAddr
  = NodeAddr { unwrap :: Integer }
  deriving (Enum, Eq, Generic, Ord, Show)
  deriving newtype (Integral, NFData, Num, Real)

data NodeId
  = Ret
  | Err
  | Addr NodeAddr
  deriving (Eq, Generic, NFData, Ord, Show)

data Node
  = BasicNode
      { next :: NodeId
      , varUpdates :: [VarUpdate]
      }
  | CondNode
      { left :: NodeId
      , right :: NodeId
      , expr :: Expr
      }
  | CallNode
      { next :: NodeId
      , functionName :: Ident
      , input :: [Expr]
      , output :: [Argument]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data VarUpdate
  = VarUpdate
      { varName :: Ident
      , ty :: ExprType
      , expr :: Expr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data Expr
  = Expr
      { ty :: ExprType
      , value :: ExprValue
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data ExprType
  = ExprTypeBool
  | ExprTypeMem
  | ExprTypeDom
  | ExprTypeHtd
  | ExprTypePms
  | ExprTypeUnit
  | ExprTypeType
  | ExprTypeToken
  | ExprTypeRelWrapper
  | ExprTypeWord
      { bits :: Integer
      }
  | ExprTypeWordArray
      { length :: Integer
      , bits :: Integer
      }
  | ExprTypeArray
      { ty :: ExprType
      , length :: Integer
      }
  | ExprTypeStruct Ident
  | ExprTypePtr ExprType
  deriving (Eq, Generic, NFData, Ord, Show)

data ExprValue
  = ExprValueVar Ident
  | ExprValueOp Op [Expr]
  | ExprValueNum Integer
  | ExprValueType ExprType
  | ExprValueSymbol Ident
  | ExprValueToken Ident
  | ExprValueSMTExpr String
  deriving (Eq, Generic, NFData, Ord, Show)

data Op
  = OpPlus
  | OpMinus
  | OpTimes
  | OpModulus
  | OpDividedBy
  | OpBWAnd
  | OpBWOr
  | OpBWXOR
  | OpAnd
  | OpOr
  | OpImplies
  | OpEquals
  | OpLess
  | OpLessEquals
  | OpSignedLess
  | OpSignedLessEquals
  | OpShiftLeft
  | OpShiftRight
  | OpCountLeadingZeroes
  | OpCountTrailingZeroes
  | OpWordReverse
  | OpSignedShiftRight
  | OpNot
  | OpBWNot
  | OpWordCast
  | OpWordCastSigned
  | OpTrue
  | OpFalse
  | OpUnspecifiedPrecond
  | OpMemUpdate
  | OpMemAcc
  | OpIfThenElse
  | OpArrayIndex
  | OpArrayUpdate
  | OpMemDom
  | OpPValid
  | OpPWeakValid
  | OpPAlignValid
  | OpPGlobalValid
  | OpPArrayValid
  | OpHTDUpdate
  | OpWordArrayAccess
  | OpWordArrayUpdate
  | OpTokenWordsAccess
  | OpTokenWordsUpdate
  | OpROData
  | OpStackWrapper
  | OpEqSelectiveWrapper
  | OpToFloatingPoint
  | OpToFloatingPointSigned
  | OpToFloatingPointUnsigned
  | OpFloatingPointCast
  | OpImpliesROData
  | OpImpliesStackEquals
  | OpStackEqualsImplies
  deriving (Eq, Generic, NFData, Ord, Show)

class TraverseTopLevelExprs a where
    traverseTopLevelLevelExprs :: Traversal' a Expr

instance TraverseTopLevelExprs Node where
    traverseTopLevelLevelExprs =
        (#_BasicNode % _2 % traversed % traverseTopLevelLevelExprs)
            `adjoin` (#_CondNode % _3)
            `adjoin` (#_CallNode % _3 % traversed)

instance TraverseTopLevelExprs VarUpdate where
    traverseTopLevelLevelExprs = castOptic #expr

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

walkExprs :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
walkExprs f expr = do
    expr' <- f expr
    flip (traverseOf #value) expr' $ \case
        ExprValueOp op args -> ExprValueOp op <$> traverse f args
        v -> return v

walkExprsI :: (Expr -> Expr) -> Expr -> Expr
walkExprsI f = runIdentity . walkExprs (fmap Identity f)

class HasVarNames a where
    varNamesOf :: Traversal' a Ident

instance HasVarNames Argument where
    varNamesOf = castOptic #name

instance HasVarNames Node where
    varNamesOf =
        (#_BasicNode % _2 % traversed % varNamesOf)
            `adjoin` (#_CondNode % _3 % varNamesOf)
            `adjoin` (#_CallNode % adjoin (_3 % traversed % varNamesOf) (_4 % traversed % varNamesOf))

instance HasVarNames VarUpdate where
    varNamesOf = #varName `adjoin` #expr % varNamesOf

instance HasVarNames Expr where
    varNamesOf = #value % (#_ExprValueVar `adjoin` (#_ExprValueOp % _2 % traversed % varNamesOf))

instance HasVarNames ExprValue where
    varNamesOf = castOptic #_ExprValueVar

renameVars :: (HasVarNames a, Applicative f) => (Ident -> f Ident) -> a -> f a
renameVars = traverseOf varNamesOf

class HasVarBindings a where
    varBindingsOf :: Traversal' a (Ident, ExprType)

instance HasVarBindings Argument where
    varBindingsOf = castOptic $ adjacently #name #ty

instance HasVarBindings Node where
    varBindingsOf = adjoin
        (#_BasicNode % _2 % traversed % varBindingsOf)
        (#_CallNode % _4 % traversed % varBindingsOf)

instance HasVarBindings VarUpdate where
    varBindingsOf = castOptic $ adjacently #varName #ty

nodeConts :: Fold Node NodeId
nodeConts = castOptic $
    (#_BasicNode % _1)
        `adjoin`(#_CondNode % (_1 `adjoin` _2))
        `adjoin` (#_CallNode % _1)
