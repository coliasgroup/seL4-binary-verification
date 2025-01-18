{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module BV.ConcreteSyntax.FastInstances
    (
    ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Monoid (Endo (..))
import Text.Megaparsec (between)

import BV.Core.Types
import BV.SMTLIB2.Parser.Attoparsec

import BV.ConcreteSyntax.FastParsing

parsePrettyPairingId :: Parser PairingId
parsePrettyPairingId = do
    asm <- ident
    hspace *> "(ASM)" *> hspace *> "<=" *> hspace
    c <- ident
    hspace *> "(C)"
    return $ PairingOf { asm, c }
  where
    ident = Ident <$> many1 (satisfy isIdentChar)
    isIdentChar c = not (isSpace c || c == '(' || c == ')')

instance ParseFileFast (SMTProofChecks ()) where
    parseFileFast = do
        blocks <- parseBlocksFileWithTypicalKeyFormat ["Problem", "Pairing"] parsePrettyPairingId $ do
            setupLen <- decimal <* endOfLine
            impsLen <- decimal <* endOfLine
            setup <- count setupLen (parseSExprWithPlaceholders <* ignoredLines)
            imps <- count impsLen (SMTProofCheckImp () <$> parseSExprWithPlaceholders <* ignoredLines)
            return $ SMTProofCheckGroup { setup, imps }
        let x = map (\(k, v) -> M.insertWith (++) k [v]) blocks
        return . SMTProofChecks . ($ M.empty) . appEndo . foldMap Endo $ x

parseSExprWithPlaceholders :: Parser SExprWithPlaceholders
parseSExprWithPlaceholders = parseGenericSExpr $
    Left <$> parseSExprPlaceholder <|> Right <$> parseAtom

parseSExprPlaceholder :: Parser SExprPlaceholder
parseSExprPlaceholder = between "{" "}" $
    try (SExprPlaceholderMemSort <$ "MemSort") <|> try (SExprPlaceholderMemDomSort <$ "MemDomSort")
