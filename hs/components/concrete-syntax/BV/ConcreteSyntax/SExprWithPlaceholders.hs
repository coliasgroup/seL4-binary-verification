{-# LANGUAGE OverloadedStrings #-}

module BV.ConcreteSyntax.SExprWithPlaceholders
    ( buildSExprWithPlaceholders
    , parseSExprWithPlaceholders
    ) where

import Data.Text.Internal.Builder (Builder)
import Text.Megaparsec

import BV.Core.Types
import BV.SMTLIB2.Builder
import BV.SMTLIB2.Parser.Megaparsec

import BV.ConcreteSyntax.Parsing

parseSExprWithPlaceholders :: Parser SExprWithPlaceholders
parseSExprWithPlaceholders = parseGenericSExpr $
    AtomOrPlaceholderAtom <$> parseAtom <|> AtomOrPlaceholderPlaceholder <$> parseSExprPlaceholder

parseSExprPlaceholder :: Parser SExprPlaceholder
parseSExprPlaceholder = between "{" "}" $
    try (SExprPlaceholderMemSort <$ "MemSort") <|> try (SExprPlaceholderMemDomSort <$ "MemDomSort")

buildSExprWithPlaceholders :: SExprWithPlaceholders -> Builder
buildSExprWithPlaceholders = buildGenericSExpr $ \case
    AtomOrPlaceholderAtom atom -> buildAtom atom
    AtomOrPlaceholderPlaceholder placeholder -> buildSExprPlaceholder placeholder

buildSExprPlaceholder :: SExprPlaceholder -> Builder
buildSExprPlaceholder placeholder = "{" <> inner <> "}"
  where
    inner = case placeholder of
        SExprPlaceholderMemSort -> "MemSort"
        SExprPlaceholderMemDomSort -> "MemDomSort"
