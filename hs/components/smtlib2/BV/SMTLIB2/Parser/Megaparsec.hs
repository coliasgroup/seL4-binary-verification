{-# LANGUAGE OverloadedStrings #-}

module BV.SMTLIB2.Parser.Megaparsec
    ( consumeAnySExprWhitespace
    , consumeSomeSExprWhitespace
    , parseAtom
    , parseGenericSExpr
    , parseSExpr
    ) where

import BV.SMTLIB2.Types

import Control.Monad (void)
import Data.Char (isSpace)
import qualified Data.Text.Lazy as TL (Text, unpack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void TL.Text

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
    , HexadecimalAtom . TL.unpack <$> ("#x" *> takeWhile1P (Just "hexadecimal digit") isValidHexadecimalAtomChar)
    , BinaryAtom . TL.unpack <$> ("#b" *> takeWhile1P (Just "binary digit") isValidBinaryAtomChar)
    , stringP
    , symbolP
    , keywordP
    ]

stringP :: Parser UncheckedAtom
stringP = StringAtom <$> (char '"' *> go)
  where
    go = do
        c <- satisfy isValidStringAtomChar
        case c of
            '"' -> do
                next <- optional (try (lookAhead anySingle))
                case next of
                    Just '"' -> char '"' *> (('"':) <$> go)
                    _ -> return []
            _ -> (c:) <$> go

keywordP :: Parser UncheckedAtom
keywordP = fmap KeywordAtom $
    ":" *> (TL.unpack <$> takeWhile1P (Just "symbol suffix character") isValidKeywordAtomChar)

symbolP :: Parser UncheckedAtom
symbolP = fmap SymbolAtom $
    (:) <$> satisfy isValidSymbolAtomFirstChar
        <*> (TL.unpack <$> takeWhileP (Just "symbol suffix character") isValidSymbolAtomSubsequentChar)
