{-# LANGUAGE OverloadedStrings #-}

module BV.SMTLIB2.Parser.Megaparsec
    ( consumeAnySExprWhitespace
    , consumeSomeSExprWhitespace
    , parseAtom
    , parseGenericSExpr
    , parseSExpr
    ) where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import BV.SMTLIB2.Types

type Parser = Parsec Void Text

skipLineComment :: Char -> Parser ()
skipLineComment prefix = char prefix *> void (takeWhileP (Just "line comment") (/= '\n'))

consumeSomeSExprWhitespace :: Parser ()
consumeSomeSExprWhitespace = skipSome $ void (takeWhile1P (Just "space") isSpace) <|> skipLineComment ';'

consumeAnySExprWhitespace :: Parser ()
consumeAnySExprWhitespace = option () consumeSomeSExprWhitespace

parseSExpr :: Parser SExpr
parseSExpr = parseGenericSExpr parseAtom

parseGenericSExpr :: Parser a -> Parser (GenericSExpr a)
parseGenericSExpr p = go
  where
    go = Atom <$> p
        <|> List <$> between (string "(" <* consumeAnySExprWhitespace)
                             (string ")")
                             (many (go <* consumeAnySExprWhitespace))

parseAtom :: Parser Atom
parseAtom = unsafeAtom <$> choice
    [ NumeralAtom <$> L.decimal
    , HexadecimalAtom . T.unpack <$> ("#x" *> takeWhile1P (Just "hexadecimal digit") isValidHexadecimalAtomChar)
    , BinaryAtom . T.unpack <$> ("#b" *> takeWhile1P (Just "binary digit") isValidBinaryAtomChar)
    , stringP
    , symbolP
    , keywordP
    ]

stringP :: Parser UncheckedAtom
stringP = StringAtom <$> (char '"' *> go)
  where
    go = do
        c <- stringCharP
        case c of
            '"' -> return ""
            '\\' -> do
                c' <- stringCharP
                case c' of
                    '\"' -> (c':) <$> go
                    _ -> (c:) . (c':) <$> go
            _ -> (c:) <$> go
    stringCharP = satisfy isValidStringAtomChar

keywordP :: Parser UncheckedAtom
keywordP = fmap KeywordAtom $
    ":" *> (T.unpack <$> takeWhile1P (Just "symbol suffix character") isValidKeywordAtomChar)

symbolP :: Parser UncheckedAtom
symbolP = fmap SymbolAtom $
    (:) <$> satisfy isValidSymbolAtomFirstChar
        <*> (T.unpack <$> takeWhileP (Just "symbol suffix character") isValidSymbolAtomSubsequentChar)
