module BV.Core.Types.Extras.Expr where

import BV.Core.Arch (archWordSizeBits)
import BV.Core.Utils
import BV.Core.Types

import Control.Exception (assert)
import Data.Bits (shiftL)
import Data.Maybe (fromJust)
import Data.Monoid (Endo (Endo, appEndo))
import Optics
import Optics.Core.Extras (is)
import GHC.Stack (HasCallStack)

boolT :: ExprType
boolT = ExprTypeBool

wordT :: Integer -> ExprType
wordT = ExprTypeWord

memT :: ExprType
memT = ExprTypeMem

domT :: ExprType
domT = ExprTypeDom

htdT :: ExprType
htdT = ExprTypeHtd

pmsT :: ExprType
pmsT = ExprTypePms

typeT :: ExprType
typeT = ExprTypeType

tokenT :: ExprType
tokenT = ExprTypeToken

structT :: Ident -> ExprType
structT = ExprTypeStruct

ptrT :: ExprType -> ExprType
ptrT = ExprTypePtr

--

isBoolT :: ExprType -> Bool
isBoolT = is #_ExprTypeBool

isWordT :: ExprType -> Bool
isWordT = is #_ExprTypeWord

isWordWithSizeT :: Integer -> ExprType -> Bool
isWordWithSizeT n = (==) (wordT n)

isMachineWordT :: ExprType -> Bool
isMachineWordT = isWordWithSizeT archWordSizeBits

toWordBitsT :: ExprType -> Integer
toWordBitsT = fromJust . preview #_ExprTypeWord

isMemT :: ExprType -> Bool
isMemT = is #_ExprTypeMem

--

wordTBits :: HasCallStack => ExprType -> Integer
wordTBits = view (expecting #_ExprTypeWord)

--

numV :: Integer -> ExprValue
numV = ExprValueNum

opV :: Op -> [Expr] -> ExprValue
opV = ExprValueOp

boolE :: ExprValue -> Expr
boolE = Expr boolT

numE :: ExprType -> Integer -> Expr
numE ty n = assert (isWordT ty) $ Expr ty (numV n)

smtExprE :: ExprType -> SExprWithPlaceholders -> Expr
smtExprE ty sexpr = Expr ty (ExprValueSMTExpr sexpr)

--

trueE :: Expr
trueE = Expr boolT (opV OpTrue [])

falseE :: Expr
falseE = Expr boolT (opV OpFalse [])

eqE :: Expr -> Expr -> Expr
eqE lhs rhs = assertTypesEqual_ lhs rhs $ Expr boolT (opV OpEquals [lhs, rhs])

andE :: Expr -> Expr -> Expr
andE lhs rhs = Expr (assertTypesEqualAnd isBoolT lhs rhs) (opV OpAnd [lhs, rhs])

orE :: Expr -> Expr -> Expr
orE lhs rhs = Expr (assertTypesEqualAnd isBoolT lhs rhs) (opV OpOr [lhs, rhs])

notE :: Expr -> Expr
notE expr = Expr (assertType isBoolT expr) (opV OpNot [expr])

impliesE :: Expr -> Expr -> Expr
impliesE lhs rhs = Expr (assertTypesEqualAnd isBoolT lhs rhs) (opV OpImplies [lhs, rhs])

ifThenElseE :: Expr -> Expr -> Expr -> Expr
ifThenElseE cond ifTrue ifFalse = assertType_ isBoolT cond $
    Expr (assertTypesEqual ifTrue ifFalse) (opV OpIfThenElse [cond, ifTrue, ifFalse])

--

plusE :: Expr -> Expr -> Expr
plusE lhs rhs = Expr (assertTypesEqualAnd isWordT lhs rhs) (opV OpPlus [lhs, rhs])

minusE :: Expr -> Expr -> Expr
minusE lhs rhs = Expr (assertTypesEqualAnd isWordT lhs rhs) (opV OpMinus [lhs, rhs])

negE :: Expr -> Expr
negE expr = numE expr.ty 0 `minusE` expr

lessWithSignednessE :: Bool -> Expr -> Expr -> Expr
lessWithSignednessE isSigned lhs rhs = assertTypesEqualAnd_ isWordT lhs rhs $
    boolE (opV (if isSigned then OpSignedLess else OpLess) [lhs, rhs])

lessEqWithSignednessE :: Bool -> Expr -> Expr -> Expr
lessEqWithSignednessE isSigned lhs rhs = assertTypesEqualAnd_ isWordT lhs rhs $
    boolE (opV (if isSigned then OpSignedLessEquals else OpLessEquals) [lhs, rhs])

lessE :: Expr -> Expr -> Expr
lessE = lessWithSignednessE False

lessSignedE :: Expr -> Expr -> Expr
lessSignedE = lessWithSignednessE True

lessEqE :: Expr -> Expr -> Expr
lessEqE = lessEqWithSignednessE False

lessEqSignedE :: Expr -> Expr -> Expr
lessEqSignedE = lessEqWithSignednessE True

bitwiseAndE :: Expr -> Expr -> Expr
bitwiseAndE lhs rhs = Expr (assertTypesEqualAnd isWordT lhs rhs) (opV OpBWAnd [lhs, rhs])

bitwiseOrE :: Expr -> Expr -> Expr
bitwiseOrE lhs rhs = Expr (assertTypesEqualAnd isWordT lhs rhs) (opV OpBWOr [lhs, rhs])

wordReverseE :: Expr -> Expr
wordReverseE x = Expr (assertType isWordT x) (opV OpWordReverse [x])

clzE :: Expr -> Expr
clzE x = Expr (assertType isWordT x) (opV OpCountLeadingZeroes [x])

--

nImpliesE :: [Expr] -> Expr -> Expr
nImpliesE xs y = foldr impliesE y xs

--

alignedE :: Integer -> Expr -> Expr
alignedE n expr = bitwiseAndE expr mask `eqE` numE ty 0
  where
    ty = assertType isWordT expr
    mask = numE ty ((1 `shiftL` fromInteger n) - 1)

castE :: ExprType -> Expr -> Expr
castE ty expr =
    if ty == expr.ty
    then expr
    else assert (isWordT ty) . assertType_ isWordT expr $ Expr ty (opV OpWordCast [expr])

--

-- TODO move?
castCToAsmE :: ExprType -> Expr -> Expr
castCToAsmE ty expr =
    assert (isWordT ty) $
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

varFromArgE :: Argument -> Expr
varFromArgE arg = varE arg.ty arg.name

wordVarE :: Integer -> Ident -> Expr
wordVarE bits = varE (wordT bits)

memAccE :: ExprType -> Expr -> Expr -> Expr
memAccE ty addr mem =
    assertType_ isMemT mem .
    assertType_ (isWordWithSizeT archWordSizeBits) addr .
    assert (isWordT ty) $
        Expr ty (opV OpMemAcc [mem, addr])

rodataE :: Expr -> Expr
rodataE mem = assertType_ isMemT mem $ boolE (opV OpROData [mem])

stackWrapperE :: Expr -> Expr -> [Expr] -> Expr
stackWrapperE sp stack except =
    assertType_ isMemT stack .
    assertType_ (isWordWithSizeT archWordSizeBits) sp .
    appEndo (foldMap (Endo . assertType_ (isWordWithSizeT archWordSizeBits)) except) $
        Expr ExprTypeRelWrapper (opV OpStackWrapper ([sp, stack] ++ except))

--

assertType :: (ExprType -> Bool) -> Expr -> ExprType
assertType p expr = assertType_ p expr expr.ty

assertType_ :: (ExprType -> Bool) -> Expr -> a -> a
assertType_ p expr = assert (p expr.ty)

assertTypesEqual :: Expr -> Expr -> ExprType
assertTypesEqual lhs rhs = assertTypesEqual_ lhs rhs lhs.ty

assertTypesEqual_ :: Expr -> Expr -> a -> a
assertTypesEqual_ lhs rhs = assert (lhs.ty == rhs.ty)

assertTypesEqualAnd :: (ExprType -> Bool) -> Expr -> Expr -> ExprType
assertTypesEqualAnd p lhs rhs = assertTypesEqualAnd_ p lhs rhs lhs.ty

assertTypesEqualAnd_ :: (ExprType -> Bool) -> Expr -> Expr -> a -> a
assertTypesEqualAnd_ p lhs rhs = assert (lhs.ty == rhs.ty && p lhs.ty)
