{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.SMTLIB2.Parser.Attoparsec
    ( consumeAnySExprWhitespace
    , consumeSomeSExprWhitespace
    , parseAtom
    , parseGenericSExpr
    , parseSExpr
    ) where

import BV.SMTLIB2.Types

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Attoparsec.Text as A
import Data.Char (isSpace)
import qualified Data.Text as T (unpack)
import Text.Megaparsec (between)

skipLineComment :: Char -> Parser ()
skipLineComment prefix = char prefix *> skipWhile (/= '\n')

consumeSomeSExprWhitespace :: Parser ()
consumeSomeSExprWhitespace = skipMany1 $ void (takeWhile1 isSpace) <|> skipLineComment ';'

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
                             (many' (go <* consumeAnySExprWhitespace))

parseAtom :: Parser Atom
parseAtom = unsafeAtom <$> choice
    [ NumeralAtom <$> decimal
    , HexadecimalAtom . T.unpack <$> ("#x" *> takeWhile1 isValidHexadecimalAtomChar)
    , BinaryAtom . T.unpack <$> ("#b" *> takeWhile1 isValidBinaryAtomChar)
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
                next <- peekChar
                case next of
                    Just '"' -> char '"' *> (('"':) <$> go)
                    _ -> return []
            _ -> (c:) <$> go

keywordP :: Parser UncheckedAtom
keywordP = fmap KeywordAtom $
    ":" *> (T.unpack <$> A.takeWhile1 isValidKeywordAtomChar)

symbolP :: Parser UncheckedAtom
symbolP = fmap SymbolAtom $
    (:) <$> satisfy isValidSymbolAtomFirstChar
        <*> (T.unpack <$> A.takeWhile isValidSymbolAtomSubsequentChar)
