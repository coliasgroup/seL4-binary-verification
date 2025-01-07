{-# LANGUAGE OverloadedStrings #-}

module BV.Parsing
    ( ParseFile (..)
    , ParseInLine (..)
    , Parser
    , parseWholeFile
    ) where

import Control.Applicative ()
import Data.Char
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Monoid (Endo (Endo, appEndo))
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Numeric (readDec, readHex)
import Optics.Core
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import BV.Inputs
import BV.Problem
import BV.Program
import Data.Bifunctor (first)

type Parser = Parsec Void T.Text

inLineSpace :: Parser ()
inLineSpace = hidden hspace

inLineLexeme :: Parser a -> Parser a
inLineLexeme = L.lexeme inLineSpace

inLineSymbol :: Text -> Parser Text
inLineSymbol = L.symbol inLineSpace

-- TODO rename to anyWord
word :: Parser Text
word = inLineLexeme $ takeWhile1P (Just "word character") (not . isSpace)

wordWith :: (Text -> Either String a) -> Parser a
wordWith f = do
    s <- word
    case f s of
        Left err -> fail err
        Right a -> return a

wordWithOr :: String -> (Text -> Maybe a) -> Parser a
wordWithOr msg f = wordWith (maybe (Left msg) Right . f)

line :: Parser a -> Parser a
line p = do
    inLineSpace
    x <- p
    lineSep
    return x

lineSep :: Parser ()
lineSep = eol *> ignoredLines

ignoredLines :: Parser ()
ignoredLines = skipMany $ inLineSpace *> optional (L.skipLineComment "#") *> eol

parseWholeFile :: ParseFile a => String -> Text -> Either String a
parseWholeFile path = first errorBundlePretty . parse (parseFile <* eof) path

class ParseFile a where
    parseFile :: Parser a

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

class ParseInBlock a where
    parseInBlock :: Parser a

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

class ParseInLine a where
    parseInLine :: Parser a

instance ParseInLine Integer where
    parseInLine = inLineLexeme $ do
        isNegative <- try $ isJust <$> optional (char '-' <|> char '~')
        isHex <- try $ isJust <$> optional "0x"
        (if isNegative then negate else id) <$> (if isHex then L.hexadecimal else L.decimal)

parseManyInLineWith :: Parser a -> Parser [a]
parseManyInLineWith p = do
    n <- parseInLine
    count (fromInteger n) p

instance ParseInLine a => ParseInLine [a] where
    parseInLine = parseManyInLineWith parseInLine

instance ParseInLine Ident where
    parseInLine = Ident . T.unpack <$> word

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

matchOp :: Text -> Maybe Op
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

type T = Program

testString = "Struct Kernel_C.finaliseSlot_ret_C 16 4\n"

ttt :: IO ()
ttt = parseTest (parseFile :: Parser T) (fromString testString)
