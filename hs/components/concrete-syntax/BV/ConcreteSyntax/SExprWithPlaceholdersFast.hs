{-# LANGUAGE OverloadedStrings #-}

module BV.ConcreteSyntax.SExprWithPlaceholdersFast
    ( parseSExprWithPlaceholdersFast
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

parseSExprWithPlaceholdersFast :: Parser SExprWithPlaceholders
parseSExprWithPlaceholdersFast = parseGenericSExpr $
    AtomOrPlaceholderAtom <$> parseAtom <|> AtomOrPlaceholderPlaceholder <$> parseSExprPlaceholderFast

parseSExprPlaceholderFast :: Parser SExprPlaceholder
parseSExprPlaceholderFast = between "{" "}" $
    try (SExprPlaceholderMemSort <$ "MemSort") <|> try (SExprPlaceholderMemDomSort <$ "MemDomSort")
