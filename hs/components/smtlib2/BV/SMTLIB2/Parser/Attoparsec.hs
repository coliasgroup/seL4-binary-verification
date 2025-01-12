{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.SMTLIB2.Parser.Attoparsec
    ( parseAtom
    , parseGenericSExpr
    , parseSExpr
    ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Attoparsec.Text as A
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Numeric (readBin)
import Text.Megaparsec (between)

import BV.SMTLIB2.Types

skipLineComment :: Char -> Parser ()
skipLineComment prefix = char prefix *> void (A.takeWhile (/= '\n'))

spaceConsumer :: Parser ()
spaceConsumer = skipMany $ void (takeWhile1 isSpace) <|> skipLineComment ';'

symbol :: Text -> Parser Text
symbol s = string s <* spaceConsumer

binary :: (Eq a, Num a) => Parser a
binary = do
    s <- T.unpack <$> A.takeWhile1 (\c -> c == '0' || c == '1')
    let [(n, "")] = readBin s
    return n

parseSExpr :: Parser SExpr
parseSExpr = parseGenericSExpr parseAtom

parseGenericSExpr :: Parser a -> Parser (GenericSExpr a)
parseGenericSExpr p = spaceConsumer *> go
  where
    go = (Atom <$> p <|> List <$> between (symbol "(") (symbol ")") (many' go)) <* spaceConsumer

parseAtom :: Parser Atom
parseAtom = unsafeAtom <$> choice
    [ NumeralAtom <$> decimal
    , HexadecimalAtom <$> ("#x" *> hexadecimal)
    , BinaryAtom <$> ("#b" *> binary)
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
            '\\' -> do
                c' <- stringCharP
                case c' of
                    '\"' -> (c':) <$> go
                    _ -> (c:) . (c':) <$> go
            _ -> (c:) <$> go
    stringCharP = satisfy isValidStringAtomChar

keywordP :: Parser UncheckedAtom
keywordP = fmap KeywordAtom $
    ":" *> (T.unpack <$> A.takeWhile1 isValidKeywordAtomChar)

symbolP :: Parser UncheckedAtom
symbolP = fmap SymbolAtom $
    (:) <$> satisfy isValidSymbolAtomFirstChar
        <*> (T.unpack <$> A.takeWhile isValidSymbolAtomSubsequentChar)
