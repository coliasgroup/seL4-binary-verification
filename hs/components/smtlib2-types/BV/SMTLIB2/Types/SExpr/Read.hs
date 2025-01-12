module BV.SMTLIB2.Types.SExpr.Read
    ( readAtom
    , readSExpr
    , readSExprs
    , tryReadAtom
    , tryReadSExpr
    , tryReadSExprs
    ) where

import Control.Applicative ((<|>))
import Data.Char (isDigit, isSpace)
import Data.Functor (void)
import Data.Maybe (fromJust)
import GHC.Natural (Natural)
import Numeric (readDec)
import Text.ParserCombinators.ReadP

import BV.SMTLIB2.Types.SExpr

tryReadSExprs :: String -> Maybe [SExpr]
tryReadSExprs = readPToTryRead $ anySExprWhitespaceP *> many (sexprP <* anySExprWhitespaceP)

tryReadSExpr :: String -> Maybe SExpr
tryReadSExpr = readPToTryRead sexprP

tryReadAtom :: String -> Maybe Atom
tryReadAtom = readPToTryRead atomP

readSExprs :: String -> [SExpr]
readSExprs = fromJust . tryReadSExprs

readSExpr :: String -> SExpr
readSExpr = fromJust . tryReadSExpr

readAtom :: String -> Atom
readAtom = fromJust . tryReadAtom

readSToRead :: ReadS a -> String -> a
readSToRead p = fromJust . readSToTryRead p

readSToTryRead :: ReadS a -> String -> Maybe a
readSToTryRead p s = case p s of
    [(a, "")] -> Just a
    [] -> Nothing
    _ -> error "unreachable"

readPToTryRead :: ReadP a -> String -> Maybe a
readPToTryRead = readSToTryRead . readP_to_S . (<* eof)

skipLineComment :: Char -> ReadP ()
skipLineComment prefix = char prefix *> void (munch (/= '\n'))

someSExprWhitespaceP :: ReadP ()
someSExprWhitespaceP = skipMany1 $ void (munch1 isSpace) <|> skipLineComment ';'

anySExprWhitespaceP :: ReadP ()
anySExprWhitespaceP = option () someSExprWhitespaceP

sexprP :: ReadP SExpr
sexprP = go
  where
    go = Atom <$> atomP
        <|> List <$> between (string "(" <* anySExprWhitespaceP)
                             (string ")")
                             (many (go <* anySExprWhitespaceP))

atomP :: ReadP Atom
atomP = unsafeAtom <$> choice
    [ NumeralAtom <$> decimalP
    , HexadecimalAtom <$> (string "#x" *> munch1 isValidHexadecimalAtomChar)
    , BinaryAtom <$> (string "#b" *> munch1 isValidBinaryAtomChar)
    , stringP
    , symbolP
    , keywordP
    ]

decimalP :: ReadP Natural
decimalP = readSToRead readDec <$> (zeroP <|> nonZeroP)
  where
    zeroP = (:[]) <$> char '0'
    nonZeroP = (:) <$> satisfy (\c -> isDigit c && c /= '0') <*> munch isDigit

stringP :: ReadP UncheckedAtom
stringP = StringAtom <$> (char '"' *> go)
  where
    go = do
        c <- stringCharP
        case c of
            '"' -> return ""
            '\\' -> do
                c' <- stringCharP
                case c' of
                    '\"' -> (c':) <$> go
                    _ -> (c:) . (c':) <$> go
            _ -> (c:) <$> go
    stringCharP = satisfy isValidStringAtomChar

keywordP :: ReadP UncheckedAtom
keywordP = fmap KeywordAtom $ char ':' *> munch1 isValidKeywordAtomChar

symbolP :: ReadP UncheckedAtom
symbolP = fmap SymbolAtom $
    (:) <$> satisfy isValidSymbolAtomFirstChar
        <*> munch isValidSymbolAtomSubsequentChar
