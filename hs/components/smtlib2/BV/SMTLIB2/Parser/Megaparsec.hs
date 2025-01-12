{-# LANGUAGE OverloadedStrings #-}

module BV.SMTLIB2.Parser.Megaparsec
    ( parseSExpr
    ) where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import BV.SMTLIB2.Parser.Common
import BV.SMTLIB2.Types

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment ";") empty

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parseSExpr :: Parser SExpr
parseSExpr = spaceConsumer *>  go
  where
    go = choice
        [ numeralSExpr <$> L.decimal
        , hexadecimalSExpr <$> ("#x" *> L.hexadecimal)
        , binarySExpr <$> ("#b" *> L.binary)
        , stringP
        , symbolP
        , keywordP
        , listSExpr <$> between (symbol "(") (symbol ")") (many go)
        ] <* spaceConsumer

trySExpr :: (a -> Maybe SExpr) -> String -> Parser a -> Parser SExpr
trySExpr constructor msg p = p >>= \x -> case constructor x of
    Just ex -> return ex
    Nothing -> fail msg

stringP :: Parser SExpr
stringP = trySExpr tryStringSExpr "invalid string constant" (single '"' *> go)
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
        <*> (T.unpack <$> takeWhileP (Just "symbol suffix character") isValidSExprKeywordChar)

keywordP :: Parser SExpr
keywordP = trySExpr tryKeywordSExpr "invalid keyword" $
    T.unpack <$> (":" *> takeWhileP (Just "symbol suffix character") isValidSExprKeywordChar)

x = "(declare-fun rodata-witness () (_ BitVec 32))"
-- x = ""

foo :: IO ()
foo = parseTest parseSExpr x
