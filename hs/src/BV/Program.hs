{-# HLINT ignore "Use newtype instead of data" #-}

module BV.Program where

import Data.Map (Map)
import GHC.Generics (Generic)
import Optics.Core

newtype Ident
  = Ident { getIdent :: String }
  deriving (Eq, Generic, Show)

data Program
  = Program
      { structs :: Map Ident Struct
      , constGlobals :: Map Ident ConstGlobal
      , functions :: Map Ident Function
      }
  deriving (Generic, Show)

data Struct
  = Struct
      { size :: Integer
      , align :: Integer
      , fields :: Map Ident StructField
      }
  deriving (Generic, Show)

data StructField
  = StructField
      { ty :: Type
      , offset :: Integer
      }
  deriving (Generic, Show)

data ConstGlobal
  = ConstGlobal
      { value :: Expr
      }
  deriving (Generic, Show)

data Function
  = Function
      { input :: [Argument]
      , output :: [Argument]
      , body :: Maybe FunctionBody
      }
  deriving (Generic, Show)

type NodeMap = Map NodeAddr Node

data FunctionBody
  = FunctionBody
      { entryPoint :: NodeID
      , nodes :: NodeMap
      }
  deriving (Generic, Show)

data Argument
  = Argument
      { name :: Ident
      , ty :: Type
      }
  deriving (Generic, Show)

newtype NodeAddr
  = NodeAddr Integer
  deriving (Eq, Generic, Ord, Show)

data NodeID
  = Ret
  | Err
  | Addr NodeAddr
  deriving (Eq, Generic, Ord, Show)

data Node
  = BasicNode
      { next :: NodeID
      , varUpdates :: [VarUpdate]
      }
  | CondNode
      { left :: NodeID
      , right :: NodeID
      , expr :: Expr
      }
  | CallNode
      { next :: NodeID
      , functionName :: Ident
      , input :: [Expr]
      , output :: [Argument]
      }
  deriving (Generic, Show)

data VarUpdate
  = VarUpdate
      { varName :: Ident
      , ty :: Type
      , expr :: Expr
      }
  deriving (Generic, Show)

data Expr
  = Expr
      { ty :: Type
      , value :: ExprValue
      }
  deriving (Generic, Show)

data Type
  = Bool
  | Mem
  | Dom
  | Htd
  | Pms
  | Unit
  | Type
  | Token
  | RelWrapper
  | Word
      { bits :: Integer
      }
  | WordArray
      { bits :: Integer
      , length :: Integer
      }
  | Array
      { ty :: Type
      , length :: Integer
      }
  | StructType Ident
  | Ptr Type
  deriving (Generic, Show)

data ExprValue
  = Var Ident
  | Op Op [Expr]
  | Num Integer
  | TypeValue Type
  | Symbol Ident
  | TokenValue Ident
  deriving (Generic, Show)

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
  deriving (Generic, Show)

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
    varNamesOf = castOptic #_Var

renameVars :: (HasVarNames a, Applicative f) => (Ident -> f Ident) -> a -> f a
renameVars = traverseOf varNamesOf

class HasVarBindings a where
    varBindingsOf :: Traversal' a (Ident, Type)

instance HasVarBindings Argument where
    varBindingsOf = castOptic $ adjacently #name #ty

instance HasVarBindings Node where
    varBindingsOf = adjoin
        (#_BasicNode % _2 % traversed % varBindingsOf)
        (#_CallNode % _4 % traversed % varBindingsOf)

instance HasVarBindings VarUpdate where
    varBindingsOf = castOptic $ adjacently #varName #ty

nodeConts :: Fold Node NodeID
nodeConts = castOptic $
    (#_BasicNode % _1)
        `adjoin`(#_CondNode % (_1 `adjoin` _2))
        `adjoin` (#_CallNode % _1)

adjacently :: Lens' s a -> Lens' s a' -> Lens' s (a, a')
adjacently l r =
    withLens l $ \getl setl ->
    withLens r $ \getr setr ->
        lens (\s -> (getl s, getr s))
             (\s (b, b') -> setr (setl s b) b')
