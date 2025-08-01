{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.Program
    ( BasicNode (..)
    , CallNode (..)
    , CondNode (..)
    , ConstGlobal (..)
    , Expr (..)
    , ExprType (..)
    , ExprValue (..)
    , Function (..)
    , FunctionBody (..)
    , Ident (..)
    , MaybeSplit (..)
    , NameTy (..)
    , Named (..)
    , Node (..)
    , NodeAddr (..)
    , NodeId (..)
    , NodeMap
    , Op (..)
    , Program (..)
    , SplitMem (..)
    , Struct (..)
    , StructField (..)
    , VarUpdate (..)
    , fromListOfNamed
    , fromNotSplit
    , prettyNodeId
    , toListOfNamed
    , withNamed
    ) where

import BV.Core.Types.SExprWithPlaceholders (SExprWithPlaceholders)
import BV.Utils (expecting)

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Map (Map)
import qualified Data.Map as M
import Data.String (IsString (..))
import GHC.Generics (Generic)
import Optics (view)
import Text.Printf (PrintfArg (..))

newtype Ident
  = Ident { unwrap :: String }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance Binary Ident where

instance IsString Ident where
    fromString = Ident

data Named a
  = Named
      { name :: Ident
      , value :: a
      }
  deriving (Eq, Functor, Generic, NFData, Ord, Show)

instance Binary a => Binary (Named a) where

toListOfNamed :: Map Ident a -> [Named a]
toListOfNamed = map (\(name, value) -> Named { name, value }) . M.toAscList

fromListOfNamed :: [Named a] -> Map Ident a
fromListOfNamed = M.fromList . map (\Named { name, value } -> (name, value))

withNamed :: (Ident -> a -> b) -> Named a -> b
withNamed f (Named named value) = f named value

data NameTy
  = NameTy
      { name :: Ident
      , ty :: ExprType
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary NameTy where

data Program
  = Program
      { structs :: Map Ident Struct
      , constGlobals :: Map Ident ConstGlobal
      , functions :: Map Ident Function
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary Program where

instance Semigroup Program where
    x <> y = Program
        { structs = x.structs <> y.structs
        , constGlobals = x.constGlobals <> y.constGlobals
        , functions = x.functions <> y.functions
        }

instance Monoid Program where
    mempty = Program
        { structs = mempty
        , constGlobals = mempty
        , functions = mempty
        }

data Struct
  = Struct
      { size :: Integer
      , align :: Integer
      , fields :: Map Ident StructField
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary Struct where

data StructField
  = StructField
      { ty :: ExprType
      , offset :: Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary StructField where

data ConstGlobal
  = ConstGlobal
      { value :: Expr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary ConstGlobal where

data Function
  = Function
      { input :: [NameTy]
      , output :: [NameTy]
      , body :: Maybe FunctionBody
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary Function where

type NodeMap = Map NodeAddr Node

data FunctionBody
  = FunctionBody
      { entryPoint :: NodeId
      , nodes :: NodeMap
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary FunctionBody where

newtype NodeAddr
  = NodeAddr { unwrap :: Integer }
  deriving (Enum, Eq, Generic, Ord, Show)
  deriving newtype (Integral, NFData, Num, Real)

instance Binary NodeAddr where

instance PrintfArg NodeAddr where
    formatArg = formatArg . (.unwrap)
    parseFormat _ = parseFormat (undefined :: Integer)

data NodeId
  = Ret
  | Err
  | Addr NodeAddr
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary NodeId where

prettyNodeId :: NodeId -> String
prettyNodeId = \case
    Ret -> "Ret"
    Err -> "Err"
    Addr (NodeAddr addr) -> show addr

data Node
  = NodeBasic BasicNode
  | NodeCond CondNode
  | NodeCall CallNode
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary Node where

data BasicNode
  = BasicNode
      { next :: NodeId
      , varUpdates :: [VarUpdate]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary BasicNode where

data CondNode
  = CondNode
      { left :: NodeId
      , right :: NodeId
      , expr :: Expr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary CondNode where

data CallNode
  = CallNode
      { next :: NodeId
      , functionName :: Ident
      , input :: [Expr]
      , output :: [NameTy]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary CallNode where

data VarUpdate
  = VarUpdate
      { var :: NameTy
      , val :: Expr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary VarUpdate where

data Expr
  = Expr
      { ty :: ExprType
      , value :: ExprValue
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary Expr where

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
      { len :: Integer
      , bits :: Integer
      }
  | ExprTypeArray
      { ty :: ExprType
      , len :: Integer
      }
  | ExprTypeStruct Ident
  | ExprTypePtr ExprType
  | ExprTypeGlobalWrapper ExprType
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary ExprType where

data ExprValue
  = ExprValueVar Ident
  | ExprValueOp Op [Expr]
  | ExprValueNum Integer
  | ExprValueType ExprType
  | ExprValueSymbol Ident
  | ExprValueToken Ident
  | ExprValueSMTExpr MaybeSplit
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary ExprValue where

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
  | OpStackEquals
  | OpImpliesStackEquals
  | OpStackEqualsImplies
  | OpMemAccWrapper
  | OpMemWrapper
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary Op where

data MaybeSplit
  = NotSplit SExprWithPlaceholders
  | Split SplitMem
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary MaybeSplit where

fromNotSplit :: MaybeSplit -> SExprWithPlaceholders
fromNotSplit = view $ expecting #_NotSplit

data SplitMem
  = SplitMem
      { split :: SExprWithPlaceholders
      , top :: SExprWithPlaceholders
      , bottom :: SExprWithPlaceholders
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary SplitMem where
