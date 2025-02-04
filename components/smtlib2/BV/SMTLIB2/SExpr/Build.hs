{-# LANGUAGE OverloadedStrings #-}

module BV.SMTLIB2.SExpr.Build
    ( buildAtom
    , buildGenericSExpr
    , buildSExpr
    , buildUncheckedAtom
    , buildUncheckedSExpr
    ) where

import BV.SMTLIB2.SExpr

import Data.Text.Lazy.Builder (Builder, fromString, singleton)
import Data.Text.Lazy.Builder.Int (decimal)

buildGenericSExpr :: (a -> Builder) -> GenericSExpr a -> Builder
buildGenericSExpr f = \case
    Atom a -> f a
    List [] -> "()"
    List (x:xs) ->
           singleton '('
        <> buildGenericSExpr f x
        <> foldMap ((singleton ' ' <>) . buildGenericSExpr f) xs
        <> singleton ')'

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
    StringAtom s -> singleton '\"' <> foldMap escapeChar s <> singleton '\"'
    SymbolAtom s -> fromString s
    KeywordAtom s -> singleton ':' <> fromString s
  where
    escapeChar = \case
        '"' -> "\"\""
        c -> singleton c
