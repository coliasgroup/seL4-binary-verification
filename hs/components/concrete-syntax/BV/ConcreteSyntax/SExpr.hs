{-# LANGUAGE OverloadedStrings #-}

module BV.ConcreteSyntax.SExpr
    ( buildSExpr
    , parseSExpr
    ) where

import Data.Char
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Text.Lazy.Builder (Builder)
import Data.Void (Void)
import qualified Data.Attoparsec.Text as A
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import BV.Core.SExpr

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment ";") empty

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parseSExpr :: Parser SExpr
parseSExpr = spaceConsumer *>  go
  where
    go = A.choice
        [ List <$> between (symbol "(") (symbol ")") (many go)
        , Atom <$> ("|" *> manyTill anySingle "|")
        , Atom . T.unpack <$> takeWhile1P Nothing isAtomChar
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
