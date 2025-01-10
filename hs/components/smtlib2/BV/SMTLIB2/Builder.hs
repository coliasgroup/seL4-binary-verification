{-# LANGUAGE OverloadedStrings #-}

module BV.SMTLIB2.Builder
    ( buildSExpr
    ) where

import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Numeric (showBin)

import BV.SMTLIB2.Types

buildSExpr :: SExpr -> Builder
buildSExpr = buildSExprView . viewSExpr

buildSExprView :: SExprView -> Builder
buildSExprView = \case
    SExprConstant c -> buildSExprConstantView c
    SExprSymbol s -> fromString s
    SExprKeyword s -> singleton ':' <> fromString s
    SExprList [] -> "()"
    SExprList (x:xs) ->
           singleton '('
        <> buildSExprView x
        <> foldMap ((singleton ' ' <>) . buildSExprView) xs
        <> singleton ')'

buildSExprConstantView :: SExprConstantView -> Builder
buildSExprConstantView = \case
    SExprConstantNumeral n -> decimal n
    SExprConstantHexadecimal n -> "#x" <> hexadecimal n
    SExprConstantBinary n -> "#b" <> fromString (showBin n "")
    SExprConstantString s -> singleton '\"' <> foldMap escapeChar s <> singleton '\"'
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = singleton c
