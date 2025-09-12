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
    , ifThenElseE
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
    , pointerE
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

numV :: Integer -> ExprValue c
numV = ExprValueNum

opV :: Op -> [Expr c] -> ExprValue c
opV = ExprValueOp

boolE :: ExprValue c -> Expr c
boolE = Expr boolT

fromBoolE :: Bool -> Expr c
fromBoolE v = if v then trueE else falseE

numE :: ExprType -> Integer -> Expr c
numE ty n = ensure (isWordT ty) $ Expr ty (numV n)

smtExprE :: ExprType -> MaybeSplit -> Expr c
smtExprE ty smt = Expr ty (ExprValueSMTExpr smt)

--

trueE :: Expr c
trueE = Expr boolT (opV OpTrue [])

falseE :: Expr c
falseE = Expr boolT (opV OpFalse [])

eqE :: Expr c -> Expr c -> Expr c
eqE lhs rhs = ensureTypesEqual_ lhs rhs $ Expr boolT (opV OpEquals [lhs, rhs])

andE :: Expr c -> Expr c -> Expr c
andE lhs rhs = Expr (ensureTypesEqualAnd isBoolT lhs rhs) (opV OpAnd [lhs, rhs])

orE :: Expr c -> Expr c -> Expr c
orE lhs rhs = Expr (ensureTypesEqualAnd isBoolT lhs rhs) (opV OpOr [lhs, rhs])

notE :: Expr c -> Expr c
notE expr = Expr (ensureType isBoolT expr) (opV OpNot [expr])

impliesE :: Expr c -> Expr c -> Expr c
impliesE lhs rhs = Expr (ensureTypesEqualAnd isBoolT lhs rhs) (opV OpImplies [lhs, rhs])

ifThenElseE :: Expr c -> Expr c -> Expr c -> Expr c
ifThenElseE cond ifTrue ifFalse = ensureType_ isBoolT cond $
    Expr (ensureTypesEqual ifTrue ifFalse) (opV OpIfThenElse [cond, ifTrue, ifFalse])

word32E :: Integer -> Expr c
word32E = numE word32T

--

plusE :: Expr c -> Expr c -> Expr c
plusE lhs rhs = Expr (ensureTypesEqualAnd isWordT lhs rhs) (opV OpPlus [lhs, rhs])

minusE :: Expr c -> Expr c -> Expr c
minusE lhs rhs = Expr (ensureTypesEqualAnd isWordT lhs rhs) (opV OpMinus [lhs, rhs])

timesE :: Expr c -> Expr c -> Expr c
timesE lhs rhs = Expr (ensureTypesEqualAnd isWordT lhs rhs) (opV OpTimes [lhs, rhs])

modulusE :: Expr c -> Expr c -> Expr c
modulusE lhs rhs = Expr (ensureTypesEqualAnd isWordT lhs rhs) (opV OpModulus [lhs, rhs])

negE :: Expr c -> Expr c
negE expr = numE expr.ty 0 `minusE` expr

lessWithSignednessE :: Bool -> Expr c -> Expr c -> Expr c
lessWithSignednessE isSigned lhs rhs = ensureTypesEqualAnd_ isWordT lhs rhs $
    boolE (opV (if isSigned then OpSignedLess else OpLess) [lhs, rhs])

lessEqWithSignednessE :: Bool -> Expr c -> Expr c -> Expr c
lessEqWithSignednessE isSigned lhs rhs = ensureTypesEqualAnd_ isWordT lhs rhs $
    boolE (opV (if isSigned then OpSignedLessEquals else OpLessEquals) [lhs, rhs])

lessE :: Expr c -> Expr c -> Expr c
lessE = lessWithSignednessE False

lessEqE :: Expr c -> Expr c -> Expr c
lessEqE = lessEqWithSignednessE False

bitwiseAndE :: Expr c -> Expr c -> Expr c
bitwiseAndE lhs rhs = Expr (ensureTypesEqualAnd isWordT lhs rhs) (opV OpBWAnd [lhs, rhs])

wordReverseE :: Expr c -> Expr c
wordReverseE x = Expr (ensureType isWordT x) (opV OpWordReverse [x])

clzE :: Expr c -> Expr c
clzE x = Expr (ensureType isWordT x) (opV OpCountLeadingZeroes [x])

--

nImpliesE :: [Expr c] -> Expr c -> Expr c
nImpliesE xs y = foldr impliesE y xs

--

alignedE :: Integer -> Expr c -> Expr c
alignedE n expr = bitwiseAndE expr mask `eqE` numE ty 0
  where
    ty = ensureType isWordT expr
    mask = numE ty ((1 `shiftL` fromIntegerChecked n) - 1)

castE :: ExprType -> Expr c -> Expr c
castE ty expr =
    if ty == expr.ty
    then expr
    else ensure (isWordT ty) . ensureType_ isWordT expr $ Expr ty (opV OpWordCast [expr])

--

-- TODO move?
castCToAsmE :: ExprType -> Expr c -> Expr c
castCToAsmE ty expr =
    ensure (isWordT ty) $
        if isBoolT expr.ty
        then ifThenElseE expr (numE ty 1) (numE ty 0)
        else castE ty expr

-- TODO move
machineWordE :: Integer -> Expr c
machineWordE = numE machineWordT

-- TODO move
machineWordVarE :: Ident -> Expr c
machineWordVarE = varE machineWordT

-- TODO move
machineWordT :: ExprType
machineWordT = wordT archWordSizeBits

--

tokenE :: Ident -> Expr c
tokenE = Expr tokenT . ExprValueToken

varE :: ExprType -> Ident -> Expr c
varE ty = Expr ty . ExprValueVar

varFromNameTyE :: NameTy -> Expr c
varFromNameTyE arg = varE arg.ty arg.name

nameTyFromVarE :: Expr c -> NameTy
nameTyFromVarE (Expr ty (ExprValueVar name)) = NameTy name ty

memAccE :: ExprType -> Expr c -> Expr c -> Expr c
memAccE ty addr mem =
    ensureType_ isMemT mem .
    ensureType_ (isWordWithSizeT archWordSizeBits) addr .
    ensure (isWordT ty) $
        Expr ty (opV OpMemAcc [mem, addr])

memUpdE :: Expr c -> Expr c -> Expr c -> Expr c
memUpdE addr mem v =
    ensureType_ isMemT mem .
    ensureType_ (isWordWithSizeT archWordSizeBits) addr .
    ensure (isWordT v.ty) $
        Expr mem.ty (opV OpMemUpdate [mem, addr, v])

rodataE :: Expr c -> Expr c
rodataE mem = ensureType_ isMemT mem $ boolE (opV (OpExt OpExtROData) [mem])

stackWrapperE :: Expr c -> Expr c -> [Expr c] -> Expr c
stackWrapperE sp stack excepts =
    ensureType_ isMemT stack .
    ensureType_ (isWordWithSizeT archWordSizeBits) sp .
    appEndo (foldMap (Endo . ensureType_ (isWordWithSizeT archWordSizeBits)) excepts) $
        Expr ExprTypeRelWrapper (opV (OpExt OpExtStackWrapper) ([sp, stack] ++ excepts))

pointerE :: ExprType -> Expr c -> Expr c
pointerE ty addr =
    ensureType_ (isWordWithSizeT archWordSizeBits) addr $
    Expr (ExprTypePtr ty) addr.value

--

ensureType :: (ExprType -> Bool) -> Expr c -> ExprType
ensureType p expr = ensureType_ p expr expr.ty

ensureType_ :: (ExprType -> Bool) -> Expr c -> a -> a
ensureType_ p expr = ensure (p expr.ty)

ensureTypesEqual :: Expr c -> Expr c -> ExprType
ensureTypesEqual lhs rhs = ensureTypesEqual_ lhs rhs lhs.ty

ensureTypesEqual_ :: Expr c -> Expr c -> a -> a
ensureTypesEqual_ lhs rhs = ensure (lhs.ty == rhs.ty)

ensureTypesEqualAnd :: (ExprType -> Bool) -> Expr c -> Expr c -> ExprType
ensureTypesEqualAnd p lhs rhs = ensureTypesEqualAnd_ p lhs rhs lhs.ty

ensureTypesEqualAnd_ :: (ExprType -> Bool) -> Expr c -> Expr c -> a -> a
ensureTypesEqualAnd_ p lhs rhs = ensure (lhs.ty == rhs.ty && p lhs.ty)

--

data MemOpKind
  = MemOpKindAcc
  | MemOpKindUpdate
  deriving (Eq, Generic, NFData, Ord, Show)

data MemOp c
  = MemOp
      { kind :: MemOpKind
      , addr :: Expr c
      , value :: Expr c
      , mem :: Expr c
      }
  deriving (Eq, Generic, NFData, Ord, Show)

getMemAccess :: AffineFold (Expr c) (MemOp c)
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

splitMemE :: Expr c -> Expr c -> Expr c -> Expr c
splitMemE addr top bottom =
    ensureType_ (isWordWithSizeT archWordSizeBits) addr .
    ensureType_ isMemT top .
    ensureType_ isMemT bottom $
        Expr top.ty (opV (OpExt OpExtSplitMem) [addr, top, bottom])
