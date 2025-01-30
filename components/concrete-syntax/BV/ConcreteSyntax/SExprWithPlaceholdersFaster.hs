{-# LANGUAGE OverloadedStrings #-}

module BV.ConcreteSyntax.SExprWithPlaceholdersFaster
    ( parseSExprWithPlaceholdersFaster
    ) where

import BV.Core.Types
import BV.SMTLIB2.Parser.Attoparsec

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Text.Megaparsec (between)

parseSExprWithPlaceholdersFaster :: Parser SExprWithPlaceholders
parseSExprWithPlaceholdersFaster = parseGenericSExpr $
    AtomOrPlaceholderAtom <$> parseAtom <|> AtomOrPlaceholderPlaceholder <$> parseSExprPlaceholderFaster

parseSExprPlaceholderFaster :: Parser SExprPlaceholder
parseSExprPlaceholderFaster = between "{" "}" $
    try (SExprPlaceholderMemSort <$ "MemSort") <|> try (SExprPlaceholderMemDomSort <$ "MemDomSort")
