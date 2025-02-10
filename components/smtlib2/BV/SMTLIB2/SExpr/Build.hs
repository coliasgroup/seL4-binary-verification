{-# LANGUAGE OverloadedStrings #-}

module BV.SMTLIB2.SExpr.Build
    ( buildAtom
    , buildGenericSExpr
    , buildSExpr
    , buildUncheckedAtom
    , buildUncheckedSExpr
    ) where

import BV.SMTLIB2.SExpr

import Data.List (intersperse)
import Data.Text.Lazy.Builder (Builder, fromString, singleton)
import Data.Text.Lazy.Builder.Int (decimal)

buildGenericSExpr :: (a -> Builder) -> GenericSExpr a -> Builder
buildGenericSExpr f = \case
    Atom a -> f a
    List xs ->
        "(" <> mconcat (intersperse " " (map (buildGenericSExpr f) xs)) <> ")"

buildSExpr :: SExpr -> Builder
buildSExpr = buildGenericSExpr buildAtom

buildUncheckedSExpr :: UncheckedSExpr -> Builder
buildUncheckedSExpr = buildGenericSExpr buildUncheckedAtom

buildAtom :: Atom -> Builder
buildAtom = buildUncheckedAtom . viewAtom

buildUncheckedAtom :: UncheckedAtom -> Builder
buildUncheckedAtom = \case
    NumeralAtom n -> decimal n
    HexadecimalAtom s -> "#x" <> fromString s
    BinaryAtom s -> "#b" <> fromString s
    StringAtom s -> "\"" <> foldMap escapeChar s <> "\""
    SymbolAtom s -> fromString s
    KeywordAtom s -> ":" <> fromString s
  where
    escapeChar = \case
        '"' -> "\"\""
        c -> singleton c
