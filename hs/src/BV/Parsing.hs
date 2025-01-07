{-# LANGUAGE OverloadedStrings #-}

module BV.Parsing where

import Control.Applicative ()
import Data.Char
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Monoid (Endo (Endo, appEndo))
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Numeric (readDec, readHex)
import Optics.Core
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import BV.Inputs
import BV.Problem
import BV.Program

type Parser = Parsec Void T.Text

tok :: Parser String
tok = some (satisfy (not . isSpace))

tokWith :: (String -> Either String a) -> Parser a
tokWith f = do
    s <- tok
    case f s of
        Left err -> fail err
        Right a -> return a

tokWithOr :: String -> (String -> Maybe a) -> Parser a
tokWithOr msg f = tokWith (maybe (Left msg) Right . f)

tokSep :: Parser ()
tokSep = hspace1

lineSep :: Parser ()
lineSep = void $ some undefined

class ParseInLine a where
    parseInLine :: Parser a

instance ParseInLine Integer where
    parseInLine = do
        isNegative <- try $ isJust <$> optional (satisfy (`elem` ("-~" :: String)))
        isHex <- try $ isJust <$> optional "0x"
        (if isNegative then negate else id) <$> (if isHex then hexadecimal else decimal)

instance ParseInLine Op where
    parseInLine = tokWithOr "unrecognized operation" matchOp

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

type T = Integer

testString = "-0x32a"

x :: IO ()
x = parseTest (parseInLine :: Parser T) (fromString testString)
