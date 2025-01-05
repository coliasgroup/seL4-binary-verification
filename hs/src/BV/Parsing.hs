module BV.Parsing where

import Data.Char
import Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Endo (Endo, appEndo))
import GHC.Generics (Generic)
import Numeric (readHex, readDec)
import Optics.Core
import Text.Parsec
import Text.Parsec.Text
import Data.String (IsString(fromString))

import BV.Inputs
import BV.Problem
import BV.Program
import Data.Functor (void)

tok :: Parser String
tok = many1 (satisfy (not . isSpace))

tokWith :: (String -> Either String a) -> Parser a
tokWith f = do
    s <- tok
    case f s of
        Left err -> fail err
        Right a -> return a

tokWithOr :: String -> (String -> Maybe a) -> Parser a
tokWithOr msg f = tokWith (maybe (Left msg) Right . f)

tokSep :: Parser ()
tokSep = void $ many1 (char ' ' <|> tab)

lineSep :: Parser ()
lineSep = void $ many1 undefined

class ParseInLine a where
    parseInLine :: Parser a

instance ParseInLine Integer where
    parseInLine = do
        isNegative <- option False (True <$ oneOf "-~")
        isHex <- option False (True <$ try (string "0x"))
        let (digitClass, readFn) = if isHex then (hexDigit, readHex) else (digit, readDec)
        digits <- many1 digitClass
        let [(n, "")] = readFn digits
        return $ (if isNegative then negate else id) n

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
