{-# LANGUAGE OverloadedStrings #-}

module BV.ConcreteSyntax.GraphLangLike.Parsing
    ( ParseFile (..)
    , ParseInBlock (..)
    , ParseInLine (..)
    , ignoredLines
    , inLineLexeme
    , inLineSymbol
    , line
    , parseWholeFile
    , unterminatedLine
    , word
    , wordWith
    , wordWithOr
    ) where

import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Maybe (isJust)
import qualified Data.Text.Lazy as TL
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void TL.Text

inLineSpace :: Parser ()
inLineSpace = hidden hspace

inLineLexeme :: Parser a -> Parser a
inLineLexeme = L.lexeme inLineSpace

inLineSymbol :: TL.Text -> Parser TL.Text
inLineSymbol = L.symbol inLineSpace

-- TODO rename to anyWord
word :: Parser String
word = inLineLexeme $ TL.unpack <$> takeWhile1P (Just "word character") (not . isSpace)

wordWith :: (String -> Either String a) -> Parser a
wordWith f = do
    s <- word
    case f s of
        Left err -> fail err
        Right a -> return a

wordWithOr :: String -> (String -> Maybe a) -> Parser a
wordWithOr msg f = wordWith (maybe (Left msg) Right . f)

unterminatedLine :: Parser a -> Parser a
unterminatedLine = (inLineSpace *>)

line :: Parser a -> Parser a
line p = unterminatedLine p <* lineSep

lineSep :: Parser ()
lineSep = eol *> ignoredLines

ignoredLines :: Parser ()
ignoredLines = skipMany $ inLineSpace *> optional (L.skipLineComment "#") *> eol

parseWholeFile :: ParseFile a => String -> TL.Text -> Either String a
parseWholeFile = parseWholeFileWith parseFile

parseWholeFileWith :: Parser a -> String -> TL.Text -> Either String a
parseWholeFileWith p path = first errorBundlePretty . parse (p <* eof) path

class ParseFile a where
    parseFile :: Parser a

class ParseInBlock a where
    parseInBlock :: Parser a

class ParseInLine a where
    parseInLine :: Parser a

instance ParseInLine Integer where
    parseInLine = inLineLexeme $ do
        isNegative <- try $ isJust <$> optional (char '-' <|> char '~')
        isHex <- try $ isJust <$> optional "0x"
        (if isNegative then negate else id) <$> (if isHex then L.hexadecimal else L.decimal)

-- TODO fail instead of throw
instance ParseInLine Int where
    parseInLine = fromInteger <$> parseInLine

parseManyInLineWith :: Parser a -> Parser [a]
parseManyInLineWith p = do
    n <- parseInLine
    count (fromInteger n) p

instance ParseInLine a => ParseInLine [a] where
    parseInLine = parseManyInLineWith parseInLine

instance (ParseInLine a, ParseInLine b) => ParseInLine (a, b) where
    parseInLine = (,) <$> parseInLine <*> parseInLine
