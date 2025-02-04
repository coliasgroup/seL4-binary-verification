{-# LANGUAGE OverloadedStrings #-}

module BV.SMTLIB2.SExpr.Parse.Megaparsec
    ( consumeAnySExprWhitespace
    , consumeSomeSExprWhitespace
    , parseAtom
    , parseGenericSExpr
    , parseSExpr
    , readSExpr
    , readSExprs
    , tryReadSExpr
    , tryReadSExprs
    ) where

import BV.SMTLIB2.SExpr

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Char (isSpace)
import qualified Data.Text.Lazy as TL (Text, pack, unpack)
import Data.Void (Void)
import GHC.Stack (HasCallStack)
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

--

tryReadWith :: Parser a -> String -> Either String a
tryReadWith p s = first errorBundlePretty $ parse p' "" (TL.pack s)
  where
    p' = consumeAnySExprWhitespace *> p <* consumeAnySExprWhitespace <* eof

tryReadSExpr :: String -> Either String SExpr
tryReadSExpr = tryReadWith parseSExpr

tryReadSExprs :: String -> Either String [SExpr]
tryReadSExprs = tryReadWith (consumeAnySExprWhitespace *> many (parseSExpr <* consumeAnySExprWhitespace))

readSExpr :: HasCallStack => String -> SExpr
readSExpr = either error id . tryReadSExpr

readSExprs :: HasCallStack => String -> [SExpr]
readSExprs = either error id . tryReadSExprs
