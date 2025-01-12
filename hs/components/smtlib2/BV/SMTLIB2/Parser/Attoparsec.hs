{-# LANGUAGE OverloadedStrings #-}

module BV.SMTLIB2.Parser.Attoparsec
    ( parseSExpr
    ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Attoparsec.Text as A
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Numeric (readBin)
import Text.Megaparsec (between)

import BV.SMTLIB2.Parser.Common
import BV.SMTLIB2.Types

skipLineComment :: Char -> Parser ()
skipLineComment prefix = char prefix *> void (A.takeWhile (/= '\n'))

spaceConsumer :: Parser ()
spaceConsumer = skipMany $ void (takeWhile1 isSpace) <|> skipLineComment ';'

symbol :: Text -> Parser Text
symbol s = string s <* spaceConsumer

parseSExpr :: Parser SExpr
parseSExpr = spaceConsumer *>  go
  where
    go = choice
        [ numeralSExpr <$> decimal
        , hexadecimalSExpr <$> ("#x" *> hexadecimal)
        , binaryP
        , stringP
        , symbolP
        , keywordP
        , listSExpr <$> between (symbol "(") (symbol ")") (many' go)
        ] <* spaceConsumer

binaryP :: Parser SExpr
binaryP = do
    "#b"
    s <- T.unpack <$> A.takeWhile (\c -> c == '0' || c == '1')
    let [(n,"")] = readBin s
    return $ binarySExpr n

trySExpr :: (a -> Maybe SExpr) -> String -> Parser a -> Parser SExpr
trySExpr constructor msg p = p >>= \x -> case constructor x of
    Just ex -> return ex
    Nothing -> fail msg

stringP :: Parser SExpr
stringP = trySExpr tryStringSExpr "invalid string constant" (char '"' *> go)
  where
    go = stringCharP >>= \c -> case c of
            '\\' -> stringCharP >>= \c' -> case c' of
                '\"' -> (c':) <$> go
                _ -> (c:) . (c':) <$> go
            _ -> (c:) <$> go
    stringCharP = satisfy $ \c -> isAscii c && isPrint c

symbolP :: Parser SExpr
symbolP = trySExpr trySymbolSExpr "invalid symbol" $
    (:) <$> satisfy (\c -> isValidSExprKeywordChar c && not (isDigit c))
        <*> (T.unpack <$> A.takeWhile isValidSExprKeywordChar)

keywordP :: Parser SExpr
keywordP = trySExpr tryKeywordSExpr "invalid keyword" $
    T.unpack <$> A.takeWhile isValidSExprKeywordChar
