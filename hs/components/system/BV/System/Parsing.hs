{-# LANGUAGE OverloadedStrings #-}

module BV.System.Parsing
    ( ParseFile (..)
    , ParseInBlock (..)
    , ParseInLine (..)
    , Parser
    , ignoredLines
    , inLineLexeme
    , inLineSymbol
    , line
    , parseBlocksFile
    , parseBlocksFileWithTypicalKeyFormat
    , parseTypicalKeyFormat
    , parseWholeFile
    , parseWholeFileWith
    , unterminatedLine
    , word
    , wordWith
    , wordWithOr
    ) where

import Control.Applicative ()
import Data.Bifunctor (first)
import Data.Char
import Data.Maybe (isJust)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

inLineSpace :: Parser ()
inLineSpace = hidden hspace

inLineLexeme :: Parser a -> Parser a
inLineLexeme = L.lexeme inLineSpace

inLineSymbol :: Text -> Parser Text
inLineSymbol = L.symbol inLineSpace

-- TODO rename to anyWord
word :: Parser String
word = inLineLexeme $ T.unpack <$> takeWhile1P (Just "word character") (not . isSpace)

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

parseWholeFile :: ParseFile a => String -> Text -> Either String a
parseWholeFile = parseWholeFileWith parseFile

parseWholeFileWith :: Parser a -> String -> Text -> Either String a
parseWholeFileWith p path = first errorBundlePretty . parse (p <* eof) path

parseBlocksFile :: Parser k -> Parser v -> Parser [(k, v)]
parseBlocksFile pk pv = do
    ignoredLines
    many $ do
        k <- pk
        "{"
        ignoredLines
        v <- pv
        "}"
        ignoredLines
        return (k, v)

parseTypicalKeyFormat :: [String] -> Parser k -> Parser k
parseTypicalKeyFormat nesting = wrap nesting
  where
    wrap :: [String] -> Parser k -> Parser k
    wrap [] pk = pk
    wrap (x:xs) pk = hspace *> fromString x *> hspace *> "(" *> wrap xs pk <* hspace <* ")" <* hspace


parseBlocksFileWithTypicalKeyFormat :: [String] -> Parser k -> Parser v -> Parser [(k, v)]
parseBlocksFileWithTypicalKeyFormat nesting = parseBlocksFile . parseTypicalKeyFormat nesting

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

parseManyInLineWith :: Parser a -> Parser [a]
parseManyInLineWith p = do
    n <- parseInLine
    count (fromInteger n) p

instance ParseInLine a => ParseInLine [a] where
    parseInLine = parseManyInLineWith parseInLine

instance (ParseInLine a, ParseInLine b) => ParseInLine (a, b) where
    parseInLine = (,) <$> parseInLine <*> parseInLine

-- type T = Program

-- testString = "Struct Kernel_C.finaliseSlot_ret_C 16 4\n"

-- ttt :: IO ()
-- ttt = parseTest (parseFile :: Parser T) (fromString testString)
