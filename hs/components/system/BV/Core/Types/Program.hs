module BV.Core.Types.Program
    ( Argument (..)
    , ConstGlobal (..)
    , Expr (..)
    , ExprType (..)
    , ExprValue (..)
    , Function (..)
    , FunctionBody (..)
    , HasExprs (..)
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
    , VarUpdate (..)
    , fromListOfNamed
    , nodeConts
    , renameVars
    , toListOfNamed
    ) where

import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics.Core

import BV.Core.Utils

newtype Ident
  = Ident { unwrapIdent :: String }
  deriving (Eq, Generic, Ord, Show)

data Named a
  = Named
      { name :: Ident
      , value :: a
      }
  deriving (Eq, Generic, Ord, Show)

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
  deriving (Eq, Generic, Ord, Show)

data Struct
  = Struct
      { size :: Integer
      , align :: Integer
      , fields :: Map Ident StructField
      }
  deriving (Eq, Generic, Ord, Show)

data StructField
  = StructField
      { ty :: ExprType
      , offset :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

data ConstGlobal
  = ConstGlobal
      { value :: Expr
      }
  deriving (Eq, Generic, Ord, Show)

data Function
  = Function
      { input :: [Argument]
      , output :: [Argument]
      , body :: Maybe FunctionBody
      }
  deriving (Eq, Generic, Ord, Show)

type NodeMap = Map NodeAddr Node

data FunctionBody
  = FunctionBody
      { entryPoint :: NodeId
      , nodes :: NodeMap
      }
  deriving (Eq, Generic, Ord, Show)

data Argument
  = Argument
      { name :: Ident
      , ty :: ExprType
      }
  deriving (Eq, Generic, Ord, Show)

newtype NodeAddr
  = NodeAddr { unwrapNodeAddr :: Integer }
  deriving (Eq, Generic, Ord, Show)

data NodeId
  = Ret
  | Err
  | Addr NodeAddr
  deriving (Eq, Generic, Ord, Show)

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
  deriving (Eq, Generic, Ord, Show)

data VarUpdate
  = VarUpdate
      { varName :: Ident
      , ty :: ExprType
      , expr :: Expr
      }
  deriving (Eq, Generic, Ord, Show)

data Expr
  = Expr
      { ty :: ExprType
      , value :: ExprValue
      }
  deriving (Eq, Generic, Ord, Show)

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
  deriving (Eq, Generic, Ord, Show)

data ExprValue
  = ExprValueVar Ident
  | ExprValueOp Op [Expr]
  | ExprValueNum Integer
  | ExprValueType ExprType
  | ExprValueSymbol Ident
  | ExprValueToken Ident
  deriving (Eq, Generic, Ord, Show)

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
  deriving (Eq, Generic, Ord, Show)

class HasExprs a where
    exprsOf :: Traversal' a Expr

instance HasExprs Node where
    exprsOf =
        (#_BasicNode % _2 % traversed % exprsOf)
            `adjoin` (#_CondNode % _3)
            `adjoin` (#_CallNode % _3 % traversed)

instance HasExprs VarUpdate where
    exprsOf = castOptic #expr

instance HasExprs Expr where
    exprsOf = castOptic simple

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
    varNamesOf = #value % varNamesOf

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
