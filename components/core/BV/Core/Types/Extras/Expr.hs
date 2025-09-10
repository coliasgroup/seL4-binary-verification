{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.Core.Types.Extras.Expr
    ( MemOp (..)
    , MemOpKind (..)
    , alignedE
    , andE
    , bitwiseAndE
    , boolE
    , boolT
    , castCToAsmE
    , castE
    , clzE
    , eqE
    , falseE
    , fromBoolE
    , getMemAccess
    , globalWrapperT
    , impliesE
    , isBoolT
    , isMachineWordT
    , isMemT
    , isWordT
    , lessE
    , lessEqE
    , machineWordE
    , machineWordT
    , machineWordVarE
    , memAccE
    , memT
    , memUpdE
    , minusE
    , modulusE
    , nImpliesE
    , nameTyFromVarE
    , negE
    , notE
    , numE
    , orE
    , plusE
    , rodataE
    , smtExprE
    , splitMemE
    , stackWrapperE
    , structT
    , timesE
    , tokenE
    , tokenT
    , trueE
    , varE
    , varFromNameTyE
    , word32E
    , word32T
    , wordReverseE
    , wordT
    , wordTBits
    ) where

import BV.Core.Arch (archWordSizeBits)
import BV.Core.Types
import BV.Utils (ensure, fromIntegerChecked, is, viewExpecting)

import Control.DeepSeq (NFData)
import Data.Bits (shiftL)
import Data.Monoid (Endo (Endo, appEndo))
import GHC.Generics (Generic)
import Optics

boolT :: ExprType
boolT = ExprTypeBool

wordT :: Integer -> ExprType
wordT = ExprTypeWord

word32T :: ExprType
word32T = wordT 32

memT :: ExprType
memT = ExprTypeMem

tokenT :: ExprType
tokenT = ExprTypeToken

structT :: Ident -> ExprType
structT = ExprTypeStruct

globalWrapperT :: ExprType -> ExprType
globalWrapperT = ExprTypeGlobalWrapper

--

isBoolT :: ExprType -> Bool
isBoolT = is #_ExprTypeBool

isWordT :: ExprType -> Bool
isWordT = is #_ExprTypeWord

isWordWithSizeT :: Integer -> ExprType -> Bool
isWordWithSizeT n = (==) (wordT n)

isMachineWordT :: ExprType -> Bool
isMachineWordT = isWordWithSizeT archWordSizeBits

isMemT :: ExprType -> Bool
isMemT = is #_ExprTypeMem

--

wordTBits :: ExprType -> Integer
wordTBits = viewExpecting #_ExprTypeWord

--

numV :: Integer -> ExprValue
numV = ExprValueNum

opV :: Op -> [Expr] -> ExprValue
opV = ExprValueOp

boolE :: ExprValue -> Expr
boolE = Expr boolT

fromBoolE :: Bool -> Expr
fromBoolE v = if v then trueE else falseE

numE :: ExprType -> Integer -> Expr
numE ty n = ensure (isWordT ty) $ Expr ty (numV n)

smtExprE :: ExprType -> MaybeSplit -> Expr
smtExprE ty smt = Expr ty (ExprValueSMTExpr smt)

--

trueE :: Expr
trueE = Expr boolT (opV OpTrue [])

falseE :: Expr
falseE = Expr boolT (opV OpFalse [])

eqE :: Expr -> Expr -> Expr
eqE lhs rhs = ensureTypesEqual_ lhs rhs $ Expr boolT (opV OpEquals [lhs, rhs])

andE :: Expr -> Expr -> Expr
andE lhs rhs = Expr (ensureTypesEqualAnd isBoolT lhs rhs) (opV OpAnd [lhs, rhs])

orE :: Expr -> Expr -> Expr
orE lhs rhs = Expr (ensureTypesEqualAnd isBoolT lhs rhs) (opV OpOr [lhs, rhs])

notE :: Expr -> Expr
notE expr = Expr (ensureType isBoolT expr) (opV OpNot [expr])

impliesE :: Expr -> Expr -> Expr
impliesE lhs rhs = Expr (ensureTypesEqualAnd isBoolT lhs rhs) (opV OpImplies [lhs, rhs])

ifThenElseE :: Expr -> Expr -> Expr -> Expr
ifThenElseE cond ifTrue ifFalse = ensureType_ isBoolT cond $
    Expr (ensureTypesEqual ifTrue ifFalse) (opV OpIfThenElse [cond, ifTrue, ifFalse])

word32E :: Integer -> Expr
word32E = numE word32T

--

plusE :: Expr -> Expr -> Expr
plusE lhs rhs = Expr (ensureTypesEqualAnd isWordT lhs rhs) (opV OpPlus [lhs, rhs])

minusE :: Expr -> Expr -> Expr
minusE lhs rhs = Expr (ensureTypesEqualAnd isWordT lhs rhs) (opV OpMinus [lhs, rhs])

timesE :: Expr -> Expr -> Expr
timesE lhs rhs = Expr (ensureTypesEqualAnd isWordT lhs rhs) (opV OpTimes [lhs, rhs])

modulusE :: Expr -> Expr -> Expr
modulusE lhs rhs = Expr (ensureTypesEqualAnd isWordT lhs rhs) (opV OpModulus [lhs, rhs])

negE :: Expr -> Expr
negE expr = numE expr.ty 0 `minusE` expr

lessWithSignednessE :: Bool -> Expr -> Expr -> Expr
lessWithSignednessE isSigned lhs rhs = ensureTypesEqualAnd_ isWordT lhs rhs $
    boolE (opV (if isSigned then OpSignedLess else OpLess) [lhs, rhs])

lessEqWithSignednessE :: Bool -> Expr -> Expr -> Expr
lessEqWithSignednessE isSigned lhs rhs = ensureTypesEqualAnd_ isWordT lhs rhs $
    boolE (opV (if isSigned then OpSignedLessEquals else OpLessEquals) [lhs, rhs])

lessE :: Expr -> Expr -> Expr
lessE = lessWithSignednessE False

lessEqE :: Expr -> Expr -> Expr
lessEqE = lessEqWithSignednessE False

bitwiseAndE :: Expr -> Expr -> Expr
bitwiseAndE lhs rhs = Expr (ensureTypesEqualAnd isWordT lhs rhs) (opV OpBWAnd [lhs, rhs])

wordReverseE :: Expr -> Expr
wordReverseE x = Expr (ensureType isWordT x) (opV OpWordReverse [x])

clzE :: Expr -> Expr
clzE x = Expr (ensureType isWordT x) (opV OpCountLeadingZeroes [x])

--

nImpliesE :: [Expr] -> Expr -> Expr
nImpliesE xs y = foldr impliesE y xs

--

alignedE :: Integer -> Expr -> Expr
alignedE n expr = bitwiseAndE expr mask `eqE` numE ty 0
  where
    ty = ensureType isWordT expr
    mask = numE ty ((1 `shiftL` fromIntegerChecked n) - 1)

castE :: ExprType -> Expr -> Expr
castE ty expr =
    if ty == expr.ty
    then expr
    else ensure (isWordT ty) . ensureType_ isWordT expr $ Expr ty (opV OpWordCast [expr])

--

-- TODO move?
castCToAsmE :: ExprType -> Expr -> Expr
castCToAsmE ty expr =
    ensure (isWordT ty) $
        if isBoolT expr.ty
        then ifThenElseE expr (numE ty 1) (numE ty 0)
        else castE ty expr

-- TODO move
machineWordE :: Integer -> Expr
machineWordE = numE machineWordT

-- TODO move
machineWordVarE :: Ident -> Expr
machineWordVarE = varE machineWordT

-- TODO move
machineWordT :: ExprType
machineWordT = wordT archWordSizeBits

--

tokenE :: Ident -> Expr
tokenE = Expr tokenT . ExprValueToken

varE :: ExprType -> Ident -> Expr
varE ty = Expr ty . ExprValueVar

varFromNameTyE :: NameTy -> Expr
varFromNameTyE arg = varE arg.ty arg.name

nameTyFromVarE :: Expr -> NameTy
nameTyFromVarE (Expr ty (ExprValueVar name)) = NameTy name ty

memAccE :: ExprType -> Expr -> Expr -> Expr
memAccE ty addr mem =
    ensureType_ isMemT mem .
    ensureType_ (isWordWithSizeT archWordSizeBits) addr .
    ensure (isWordT ty) $
        Expr ty (opV OpMemAcc [mem, addr])

memUpdE :: Expr -> Expr -> Expr -> Expr
memUpdE addr mem v =
    ensureType_ isMemT mem .
    ensureType_ (isWordWithSizeT archWordSizeBits) addr .
    ensure (isWordT v.ty) $
        Expr mem.ty (opV OpMemUpdate [mem, addr, v])

rodataE :: Expr -> Expr
rodataE mem = ensureType_ isMemT mem $ boolE (opV (OpExt OpExtROData) [mem])

stackWrapperE :: Expr -> Expr -> [Expr] -> Expr
stackWrapperE sp stack excepts =
    ensureType_ isMemT stack .
    ensureType_ (isWordWithSizeT archWordSizeBits) sp .
    appEndo (foldMap (Endo . ensureType_ (isWordWithSizeT archWordSizeBits)) excepts) $
        Expr ExprTypeRelWrapper (opV (OpExt OpExtStackWrapper) ([sp, stack] ++ excepts))

--

ensureType :: (ExprType -> Bool) -> Expr -> ExprType
ensureType p expr = ensureType_ p expr expr.ty

ensureType_ :: (ExprType -> Bool) -> Expr -> a -> a
ensureType_ p expr = ensure (p expr.ty)

ensureTypesEqual :: Expr -> Expr -> ExprType
ensureTypesEqual lhs rhs = ensureTypesEqual_ lhs rhs lhs.ty

ensureTypesEqual_ :: Expr -> Expr -> a -> a
ensureTypesEqual_ lhs rhs = ensure (lhs.ty == rhs.ty)

ensureTypesEqualAnd :: (ExprType -> Bool) -> Expr -> Expr -> ExprType
ensureTypesEqualAnd p lhs rhs = ensureTypesEqualAnd_ p lhs rhs lhs.ty

ensureTypesEqualAnd_ :: (ExprType -> Bool) -> Expr -> Expr -> a -> a
ensureTypesEqualAnd_ p lhs rhs = ensure (lhs.ty == rhs.ty && p lhs.ty)

--

data MemOpKind
  = MemOpKindAcc
  | MemOpKindUpdate
  deriving (Eq, Generic, NFData, Ord, Show)

data MemOp
  = MemOp
      { kind :: MemOpKind
      , addr :: Expr
      , value :: Expr
      , mem :: Expr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

getMemAccess :: AffineFold Expr MemOp
getMemAccess = afolding $ \expr -> case expr.value of
    ExprValueOp OpMemAcc [mem, addr] -> Just $ MemOp
        { kind = MemOpKindAcc
        , addr
        , value = expr
        , mem
        }
    ExprValueOp OpMemUpdate [mem, addr, value] -> Just $ MemOp
        { kind = MemOpKindUpdate
        , addr
        , value
        , mem
        }
    _ -> Nothing

-- experimental

splitMemE :: Expr -> Expr -> Expr -> Expr
splitMemE addr top bottom =
    ensureType_ (isWordWithSizeT archWordSizeBits) addr .
    ensureType_ isMemT top .
    ensureType_ isMemT bottom $
        Expr top.ty (opV (OpExt OpExtSplitMem) [addr, top, bottom])
