{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <$>" #-}

module BV.Program
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

import Control.Applicative (many, optional, (<|>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.String (fromString)
import GHC.Generics (Generic)
import Optics.Core
import Text.Megaparsec (manyTill_, try)

import BV.Parsing
import BV.Printing

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

adjacently :: Lens' s a -> Lens' s a' -> Lens' s (a, a')
adjacently l r =
    withLens l $ \getl setl ->
    withLens r $ \getr setr ->
        lens (\s -> (getl s, getr s))
             (\s (b, b') -> setr (setl s b) b')

--

instance ParseFile Program where
    parseFile = do
        ignoredLines
        go $ Program
            { structs = M.empty
            , constGlobals = M.empty
            , functions = M.empty
            }
      where
        go acc =
            ((try (parseAndUpdate #structs acc)
                <|> try (parseAndUpdate #constGlobals acc)
                <|> try (parseAndUpdate #functions acc))
                    >>= go)
                        <|> return acc
        parseAndUpdate field acc = insertItemInto field acc <$> parseInBlock
        insertItemInto field acc (Named name item) = acc & field % at name ?~ item

instance ParseInBlock (Named Struct) where
    parseInBlock = do
        (name, size, align) <- line $ do
            _ <- inLineSymbol "Struct"
            name <- parseInLine
            size <- parseInLine
            align <- parseInLine
            return (name, size, align)
        fields <- many . line $ do
            _ <- inLineSymbol "StructField"
            fieldName <- parseInLine
            ty <- parseInLine
            offset <- parseInLine
            return $ Named fieldName (StructField ty offset)
        return $ Named name (Struct { size, align, fields = fromListOfNamed fields })

instance ParseInBlock (Named ConstGlobal) where
    parseInBlock = do
            _ <- inLineSymbol "ConstGlobalDef"
            undefined

instance ParseInBlock (Named Function) where
    parseInBlock = do
        (name, input, output) <- line $ do
            _ <- inLineSymbol "Function"
            name <- parseInLine
            input <- parseInLine
            output <- parseInLine
            return (name, input, output)
        body <- optional . try $ do
            (nodes, entryPoint) <- manyTill_ nodeLine (try entryPointLine)
            return $ FunctionBody { entryPoint, nodes = M.fromList nodes }
        return $ Named name (Function { input, output, body })
      where
        nodeLine = line $ (,) <$> parseInLine <*> parseInLine
        entryPointLine = line $ inLineSymbol "EntryPoint" *> parseInLine

instance ParseInLine Ident where
    parseInLine = Ident <$> word

instance ParseInLine Argument where
    parseInLine = Argument <$> parseInLine <*> parseInLine

instance ParseInLine NodeId where
    parseInLine =
        (Addr <$> try parseInLine)
            <|> (Ret <$ try (inLineSymbol "Err"))
            <|> (Ret <$ try (inLineSymbol "Ret"))

instance ParseInLine NodeAddr where
    parseInLine = NodeAddr <$> parseInLine

instance ParseInLine Node where
    parseInLine = do
        w <- word
        case w of
            "Basic" -> BasicNode <$> parseInLine <*> parseInLine
            "Cond" -> CondNode <$> parseInLine <*> parseInLine <*> parseInLine
            "Call" -> CallNode <$> parseInLine <*> parseInLine <*> parseInLine <*> parseInLine
            _ -> fail "invalid node type"

instance ParseInLine VarUpdate where
    parseInLine = VarUpdate <$> parseInLine <*> parseInLine <*> parseInLine

instance ParseInLine Expr where
    parseInLine = do
        w <- word
        case w of
            "Var" -> typical ExprValueVar
            "Op" -> do
                op <- parseInLine
                ty <- parseInLine
                args <- parseInLine
                return $ Expr { ty, value = ExprValueOp op args }
            "Num" -> typical ExprValueNum
            "Type" -> do
                ty' <- parseInLine
                return $ Expr { ty = ExprTypeType, value = ExprValueType ty' }
            "Symbol" -> typical ExprValueSymbol
            "Token" -> typical ExprValueToken
            _ -> fail "invalid value"
      where
        typical f = do
            value <- f <$> parseInLine
            ty <- parseInLine
            return $ Expr { ty, value }

instance ParseInLine ExprType where
    parseInLine = do
        w <- word
        case w of
            "Bool" -> return ExprTypeBool
            "Mem" -> return ExprTypeMem
            "Dom" -> return ExprTypeDom
            "HTD" -> return ExprTypeHtd
            "PMS" -> return ExprTypePms
            "UNIT" -> return ExprTypeUnit
            "Type" -> return ExprTypeType
            "Token" -> return ExprTypeToken
            "RelWrapper" -> return ExprTypeRelWrapper
            "Word" -> ExprTypeWord <$> parseInLine
            "WordArray" -> ExprTypeWordArray <$> parseInLine <*> parseInLine
            "Array" -> ExprTypeArray <$> parseInLine <*> parseInLine
            "Struct" -> ExprTypeStruct <$> parseInLine
            "Ptr" -> ExprTypePtr <$> parseInLine
            _ -> fail "invalid type"

instance ParseInLine Op where
    parseInLine = wordWithOr "invalid operation" matchOp

matchOp :: String -> Maybe Op
matchOp s = case s of
    "Plus" -> Just OpPlus
    "Minus" -> Just OpMinus
    "Times" -> Just OpTimes
    "Modulus" -> Just OpModulus
    "DividedBy" -> Just OpDividedBy
    "BWAnd" -> Just OpBWAnd
    "BWOr" -> Just OpBWOr
    "BWXOR" -> Just OpBWXOR
    "And" -> Just OpAnd
    "Or" -> Just OpOr
    "Implies" -> Just OpImplies
    "Equals" -> Just OpEquals
    "Less" -> Just OpLess
    "LessEquals" -> Just OpLessEquals
    "SignedLess" -> Just OpSignedLess
    "SignedLessEquals" -> Just OpSignedLessEquals
    "ShiftLeft" -> Just OpShiftLeft
    "ShiftRight" -> Just OpShiftRight
    "CountLeadingZeroes" -> Just OpCountLeadingZeroes
    "CountTrailingZeroes" -> Just OpCountTrailingZeroes
    "WordReverse" -> Just OpWordReverse
    "SignedShiftRight" -> Just OpSignedShiftRight
    "Not" -> Just OpNot
    "BWNot" -> Just OpBWNot
    "WordCast" -> Just OpWordCast
    "WordCastSigned" -> Just OpWordCastSigned
    "True" -> Just OpTrue
    "False" -> Just OpFalse
    "UnspecifiedPrecond" -> Just OpUnspecifiedPrecond
    "MemUpdate" -> Just OpMemUpdate
    "MemAcc" -> Just OpMemAcc
    "IfThenElse" -> Just OpIfThenElse
    "ArrayIndex" -> Just OpArrayIndex
    "ArrayUpdate" -> Just OpArrayUpdate
    "MemDom" -> Just OpMemDom
    "PValid" -> Just OpPValid
    "PWeakValid" -> Just OpPWeakValid
    "PAlignValid" -> Just OpPAlignValid
    "PGlobalValid" -> Just OpPGlobalValid
    "PArrayValid" -> Just OpPArrayValid
    "HTDUpdate" -> Just OpHTDUpdate
    "WordArrayAccess" -> Just OpWordArrayAccess
    "WordArrayUpdate" -> Just OpWordArrayUpdate
    "TokenWordsAccess" -> Just OpTokenWordsAccess
    "TokenWordsUpdate" -> Just OpTokenWordsUpdate
    "ROData" -> Just OpROData
    "StackWrapper" -> Just OpStackWrapper
    "EqSelectiveWrapper" -> Just OpEqSelectiveWrapper
    "ToFloatingPoint" -> Just OpToFloatingPoint
    "ToFloatingPointSigned" -> Just OpToFloatingPointSigned
    "ToFloatingPointUnsigned" -> Just OpToFloatingPointUnsigned
    "FloatingPointCast" -> Just OpFloatingPointCast
    _ -> Nothing

--

instance BuildToFile Program where
    buildToFile (Program { structs, constGlobals, functions }) =
        intersperse "\n" . map buildBlock $
            map buildInBlock (toListOfNamed structs)
                <> map buildInBlock (toListOfNamed constGlobals)
                <> map buildInBlock (toListOfNamed functions)

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
