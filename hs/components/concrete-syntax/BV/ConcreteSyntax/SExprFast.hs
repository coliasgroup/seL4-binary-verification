{-# LANGUAGE OverloadedStrings #-}

module BV.ConcreteSyntax.SExprFast
    ( parseSExprFast
    ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Attoparsec.Text as A
import Data.Char
import Data.Text as T
import Text.Megaparsec (between)

import BV.Core.SExpr

skipLineComment :: Char -> Parser ()
skipLineComment prefix = char prefix *> void (A.takeWhile (/= '\n'))

spaceConsumer :: Parser ()
spaceConsumer = skipMany $ void (takeWhile1 isSpace) <|> skipLineComment ';'

symbol :: Text -> Parser Text
symbol s = string s <* spaceConsumer

parseSExprFast :: Parser SExpr
parseSExprFast = spaceConsumer *>  go
  where
    go = choice
        [ List <$> between (symbol "(") (symbol ")") (many' go)
        , Atom <$> ("|" *> manyTill anyChar "|")
        , Atom . T.unpack <$> takeWhile1 isAtomChar
        ] <* spaceConsumer

isAtomChar :: Char -> Bool
isAtomChar c = isPrint c && not (isSpace c) && c /= ')' && c /= '"'
