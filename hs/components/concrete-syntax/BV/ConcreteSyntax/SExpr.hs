{-# LANGUAGE OverloadedStrings #-}

module BV.ConcreteSyntax.SExpr
    ( buildSExpr
    , parseSExpr
    ) where

import Data.Char hiding (char)
import qualified Data.DList as D
import Data.String (IsString, fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)
import Data.Void (Void)

import BV.Core.SExpr

-- import Text.Megaparsec
-- import Text.Megaparsec.Char
-- import qualified Text.Megaparsec.Char.Lexer as L

-- type Parser = Parsec Void T.Text

-- spaceConsumer :: Parser ()
-- spaceConsumer = L.space space1 (L.skipLineComment ";") empty

-- symbol :: T.Text -> Parser T.Text
-- symbol = L.symbol spaceConsumer

-- parseSExpr :: Parser SExpr
-- parseSExpr = spaceConsumer *>  go
--   where
--     go = choice
--         [ List <$> between (symbol "(") (symbol ")") (many go)
--         , Atom <$> ("|" *> manyTill anySingle "|")
--         , Atom <$> many (satisfy isAtomChar)
--         ] <* spaceConsumer

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Attoparsec.Text as A
import Text.Megaparsec (between)

skipLineComment :: Char -> Parser ()
skipLineComment prefix = char prefix *> void (A.takeWhile (/= '\n'))

spaceConsumer :: Parser ()
spaceConsumer =
  skipMany $ void (many1 (satisfy isSpace)) <|> skipLineComment ';'

symbol :: T.Text -> Parser T.Text
symbol s = string s <* spaceConsumer

parseSExpr :: Parser SExpr
parseSExpr = spaceConsumer *>  go
  where
    go = choice
        [ List <$> between (symbol "(") (symbol ")") (many' go)
        , Atom <$> ("|" *> manyTill anyChar "|")
        , Atom <$> many1' (satisfy isAtomChar)
        ] <* spaceConsumer

isAtomChar :: Char -> Bool
isAtomChar c = isPrint c && not (isSpace c) && c /= ')' && c /= '"'

buildSExpr :: SExpr -> Builder
buildSExpr = \case
    Atom x -> buildAtom x
    List [] -> "()"
    List (x:xs) ->
        "(" <> buildSExpr x
            <> foldr (\x' acc -> " " <> buildSExpr x' <> acc) ")" xs

buildAtom :: String -> Builder
buildAtom s =
    if not (all isSupportedAtomChar s)
    then error $ "unsupported atom: " ++ show s
    else fromString s

isSupportedAtomChar :: Char -> Bool
isSupportedAtomChar c = isPrint c && not (isSpace c) && c /= ')' && c /= '"' && c /= '|'
