{-# LANGUAGE OverloadedStrings #-}

module BV.ConcreteSyntax.FastParsing
    ( ParseFileFast (..)
    , hspace
    , hspace1
    , ignoredLines
    , parseBlocksFile
    , parseBlocksFileWithTypicalKeyFormat
    , parseTypicalKeyFormat
    , parseWholeFileFast
    , parseWholeFileFastWith
    ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor (void)
import Data.String (fromString)
import Data.Text as T

class ParseFileFast a where
    parseFileFast :: Parser a

parseWholeFileFast :: ParseFileFast a => T.Text -> Either String a
parseWholeFileFast = parseWholeFileFastWith parseFileFast

parseWholeFileFastWith :: Parser a -> T.Text -> Either String a
parseWholeFileFastWith p = parseOnly (p <* endOfInput)

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
parseTypicalKeyFormat = wrap
  where
    wrap :: [String] -> Parser k -> Parser k
    wrap [] pk = pk
    wrap (x:xs) pk = hspace *> fromString x *> hspace *> "(" *> wrap xs pk <* hspace <* ")" <* hspace

parseBlocksFileWithTypicalKeyFormat :: [String] -> Parser k -> Parser v -> Parser [(k, v)]
parseBlocksFileWithTypicalKeyFormat nesting = parseBlocksFile . parseTypicalKeyFormat nesting

hspace :: Parser ()
hspace = void $ many (satisfy isHorizontalSpace)

hspace1 :: Parser ()
hspace1 = void $ many1 (satisfy isHorizontalSpace)

ignoredLines :: Parser ()
ignoredLines = skipMany $ hspace *> endOfLine
