{-# LANGUAGE OverloadedStrings #-}

module BV.ConcreteSyntax.SExprWithPlaceholders
    ( buildSExprWithPlaceholders
    , parseSExprWithPlaceholders
    ) where

import BV.Core.Types
import BV.SMTLIB2.Parser.Megaparsec
import BV.SMTLIB2.Types.SExpr.Build

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
buildSExprWithPlaceholders = buildGenericSExpr $ \case
    AtomOrPlaceholderAtom atom -> buildAtom atom
    AtomOrPlaceholderPlaceholder placeholder -> buildSExprPlaceholder placeholder

buildSExprPlaceholder :: SExprPlaceholder -> Builder
buildSExprPlaceholder placeholder = "{" <> inner <> "}"
  where
    inner = case placeholder of
        SExprPlaceholderMemSort -> "MemSort"
        SExprPlaceholderMemDomSort -> "MemDomSort"
