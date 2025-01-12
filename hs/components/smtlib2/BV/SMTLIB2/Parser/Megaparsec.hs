{-# LANGUAGE OverloadedStrings #-}

module BV.SMTLIB2.Parser.Megaparsec
    ( parseAtom
    , parseGenericSExpr
    , parseSExpr
    ) where

import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import BV.SMTLIB2.Types

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment ";") empty

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parseSExpr :: Parser SExpr
parseSExpr = parseGenericSExpr parseAtom

parseGenericSExpr :: Parser a -> Parser (GenericSExpr a)
parseGenericSExpr p = spaceConsumer *> go
  where
    go = (Atom <$> p <|> List <$> between (symbol "(") (symbol ")") (many go)) <* spaceConsumer

parseAtom :: Parser Atom
parseAtom = unsafeAtom <$> choice
    [ NumeralAtom <$> L.decimal
    , HexadecimalAtom <$> ("#x" *> L.hexadecimal)
    , BinaryAtom <$> ("#b" *> L.binary)
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
    ":" *> (T.unpack <$> takeWhile1P (Just "symbol suffix character") isValidKeywordAtomChar)

symbolP :: Parser UncheckedAtom
symbolP = fmap SymbolAtom $
    (:) <$> satisfy isValidSymbolAtomFirstChar
        <*> (T.unpack <$> takeWhileP (Just "symbol suffix character") isValidSymbolAtomSubsequentChar)
