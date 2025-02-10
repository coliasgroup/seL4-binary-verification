{-# LANGUAGE OverloadedStrings #-}

module BV.ConcreteSyntax.SExprWithPlaceholders
    ( buildAtomOrPlaceholder
    , buildSExprWithPlaceholders
    , parseSExprWithPlaceholders
    ) where

import BV.Core.Types
import BV.SMTLIB2.SExpr.Build
import BV.SMTLIB2.SExpr.Parse.Megaparsec

import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Void (Void)
import Text.Megaparsec

type Parser = Parsec Void TL.Text

parseSExprWithPlaceholders :: Parser SExprWithPlaceholders
parseSExprWithPlaceholders = parseGenericSExpr $
    AtomOrPlaceholderAtom <$> parseAtom <|> AtomOrPlaceholderPlaceholder <$> parseSExprPlaceholder

parseSExprPlaceholder :: Parser SExprPlaceholder
parseSExprPlaceholder = between "{" "}" $
    try (SExprPlaceholderMemSort <$ "MemSort") <|> try (SExprPlaceholderMemDomSort <$ "MemDomSort")

buildSExprWithPlaceholders :: SExprWithPlaceholders -> Builder
buildSExprWithPlaceholders = buildGenericSExpr buildAtomOrPlaceholder

buildAtomOrPlaceholder :: AtomOrPlaceholder -> Builder
buildAtomOrPlaceholder = \case
    AtomOrPlaceholderAtom atom -> buildAtom atom
    AtomOrPlaceholderPlaceholder placeholder -> buildSExprPlaceholder placeholder

buildSExprPlaceholder :: SExprPlaceholder -> Builder
buildSExprPlaceholder placeholder = "{" <> inner <> "}"
  where
    inner = case placeholder of
        SExprPlaceholderMemSort -> "MemSort"
        SExprPlaceholderMemDomSort -> "MemDomSort"
