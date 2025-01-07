{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.Printing
    ( BlockBuilder
    , BuildInBlock (..)
    , BuildInLine (..)
    , BuildToFile (..)
    , LineBuilder
    , buildBlock
    , buildFile
    , buildLine
    , intersperse
    , lineInBlock
    , put
    , putDec
    , putHex
    , putManyWith
    , putWord
    ) where

import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.Monoid
import Data.String (IsString (fromString))
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)

import BV.Program

intersperse :: Monoid a => a -> [a] -> a
intersperse _ [] = mempty
intersperse sep (x:xs) = x <> mconcat (map (sep <>) xs)

newtype DList a
  = DList { applyDList :: Endo [a] }
  deriving (Monoid, Semigroup)

singleton :: a -> DList a
singleton = DList . Endo . (:)

fromList :: [a] -> DList a
fromList = DList . Endo . (++)

toList :: DList a -> [a]
toList = ($ []) . appEndo . (.applyDList)

--

class BuildToFile a where
    buildToFile :: a -> Builder

buildFile :: BuildToFile a => a -> L.Text
buildFile = toLazyText . buildToFile

instance BuildToFile Program where
    buildToFile (Program { structs, constGlobals, functions }) =
        intersperse "\n" . map buildBlock $
            map buildInBlock (toListOfNamed structs)
                <> map buildInBlock (toListOfNamed constGlobals)
                <> map buildInBlock (toListOfNamed functions)

newtype BlockBuilder
  = BlockBuilder { unwrapBlockBuilder :: DList LineBuilder }
  deriving (Monoid, Semigroup)

buildBlock :: BlockBuilder -> Builder
buildBlock blockBuilder = mconcat (map buildLine (toList blockBuilder.unwrapBlockBuilder))

lineInBlock :: LineBuilder -> BlockBuilder
lineInBlock = BlockBuilder . singleton

class BuildInBlock a where
    buildInBlock :: a -> BlockBuilder

instance BuildInBlock (Named Struct) where
    buildInBlock (Named name (Struct { size, align, fields })) =
        lineInBlock ("Struct" <> put name <> putDec size <> putDec align)
            <> mconcat (map buildField (M.toList fields))
      where
        buildField (fieldName, StructField { ty, offset }) = lineInBlock $
            "StructField" <> put fieldName <> put ty <> putDec offset

instance BuildInBlock (Named ConstGlobal) where
    buildInBlock = undefined

instance BuildInBlock (Named Function) where
    buildInBlock (Named name (Function { input, output, body })) =
        lineInBlock ("Function" <> put name <> put input <> put output)
            <> mconcat (maybeToList (buildBody <$> body))
      where
        buildBody (FunctionBody { entryPoint, nodes }) =
            mconcat (map buildNode (M.toList nodes))
                <> lineInBlock ("EntryPoint" <> put entryPoint)
        buildNode (addr, node) = lineInBlock $ put addr <> put node

newtype LineBuilder
  = LineBuilder { unwrapLineBuilder :: DList Builder }
  deriving (Monoid, Semigroup)

instance IsString LineBuilder where
    fromString = putWord

buildLine :: LineBuilder -> Builder
buildLine lineBuilder = intersperse " " (toList lineBuilder.unwrapLineBuilder) <> "\n"

put :: BuildInLine a => a -> LineBuilder
put = buildInLine

putWord :: String -> LineBuilder
putWord = putBuilder . fromString

putBuilder :: Builder -> LineBuilder
putBuilder = LineBuilder . singleton

putDec :: Integral a => a -> LineBuilder
putDec = putBuilder . decimal

putHex :: Integral a => a -> LineBuilder
putHex = putBuilder . ("0x" <>). hexadecimal

putManyWith :: (a -> LineBuilder) -> [a] -> LineBuilder
putManyWith f xs = putDec (length xs) <> mconcat (map f xs)

class BuildInLine a where
    buildInLine :: a -> LineBuilder

instance BuildInLine a => BuildInLine [a] where
    buildInLine = putManyWith buildInLine

instance BuildInLine Ident where
    buildInLine = fromString . (.unwrapIdent)

instance BuildInLine Argument where
    buildInLine (Argument { name, ty }) = put name <> put ty

instance BuildInLine NodeId where
    buildInLine Ret = "Ret"
    buildInLine Err = "Err"
    buildInLine (Addr addr) = put addr

instance BuildInLine NodeAddr where
    buildInLine= putHex . (.unwrapNodeAddr)

instance BuildInLine Node where
    buildInLine (BasicNode { next, varUpdates }) = "Basic" <> put next <> put varUpdates
    buildInLine (CondNode { left, right, expr }) = "Cond" <> put left <> put right <> put expr
    buildInLine (CallNode { next, functionName, input, output }) = "Call" <> put next <> put functionName <> put input <> put output

instance BuildInLine VarUpdate where
    buildInLine (VarUpdate { varName, ty, expr }) = put varName <> put ty <> put expr

instance BuildInLine Expr where
    buildInLine (Expr { ty, value }) = case value of
        ExprValueVar ident -> "Var" <> put ident <> put ty
        ExprValueOp op args -> "Op" <> put op <> put ty <> put args
        ExprValueNum n -> "Num" <> putHex n <> put ty
        ExprValueType ty' -> "Type" <> put ty'
        ExprValueSymbol ident -> "Symbol" <> put ident <> put ty
        ExprValueToken ident -> "Token" <> put ident <> put ty

instance BuildInLine ExprType where
    buildInLine a = case a of
        ExprTypeBool -> "Bool"
        ExprTypeMem -> "Mem"
        ExprTypeDom -> "Dom"
        ExprTypeHtd -> "HTD"
        ExprTypePms -> "PMS"
        ExprTypeUnit -> "UNIT"
        ExprTypeType -> "Type"
        ExprTypeToken -> "Token"
        ExprTypeRelWrapper -> "RelWrapper"
        ExprTypeWord { bits } -> "Word" <> putDec bits
        ExprTypeWordArray { length, bits } -> "WordArray" <> putDec length <> putDec bits
        ExprTypeArray { ty, length } -> "Array" <> put ty <> putDec length
        ExprTypeStruct ident -> "Struct" <> put ident
        ExprTypePtr ty -> "Ptr" <> put ty

instance BuildInLine Op where
    buildInLine a = case a of
        OpPlus -> "Plus"
        OpMinus -> "Minus"
        OpTimes -> "Times"
        OpModulus -> "Modulus"
        OpDividedBy -> "DividedBy"
        OpBWAnd -> "BWAnd"
        OpBWOr -> "BWOr"
        OpBWXOR -> "BWXOR"
        OpAnd -> "And"
        OpOr -> "Or"
        OpImplies -> "Implies"
        OpEquals -> "Equals"
        OpLess -> "Less"
        OpLessEquals -> "LessEquals"
        OpSignedLess -> "SignedLess"
        OpSignedLessEquals -> "SignedLessEquals"
        OpShiftLeft -> "ShiftLeft"
        OpShiftRight -> "ShiftRight"
        OpCountLeadingZeroes -> "CountLeadingZeroes"
        OpCountTrailingZeroes -> "CountTrailingZeroes"
        OpWordReverse -> "WordReverse"
        OpSignedShiftRight -> "SignedShiftRight"
        OpNot -> "Not"
        OpBWNot -> "BWNot"
        OpWordCast -> "WordCast"
        OpWordCastSigned -> "WordCastSigned"
        OpTrue -> "True"
        OpFalse -> "False"
        OpUnspecifiedPrecond -> "UnspecifiedPrecond"
        OpMemUpdate -> "MemUpdate"
        OpMemAcc -> "MemAcc"
        OpIfThenElse -> "IfThenElse"
        OpArrayIndex -> "ArrayIndex"
        OpArrayUpdate -> "ArrayUpdate"
        OpMemDom -> "MemDom"
        OpPValid -> "PValid"
        OpPWeakValid -> "PWeakValid"
        OpPAlignValid -> "PAlignValid"
        OpPGlobalValid -> "PGlobalValid"
        OpPArrayValid -> "PArrayValid"
        OpHTDUpdate -> "HTDUpdate"
        OpWordArrayAccess -> "WordArrayAccess"
        OpWordArrayUpdate -> "WordArrayUpdate"
        OpTokenWordsAccess -> "TokenWordsAccess"
        OpTokenWordsUpdate -> "TokenWordsUpdate"
        OpROData -> "ROData"
        OpStackWrapper -> "StackWrapper"
        OpEqSelectiveWrapper -> "EqSelectiveWrapper"
        OpToFloatingPoint -> "ToFloatingPoint"
        OpToFloatingPointSigned -> "ToFloatingPointSigned"
        OpToFloatingPointUnsigned -> "ToFloatingPointUnsigned"
        OpFloatingPointCast -> "FloatingPointCast"
