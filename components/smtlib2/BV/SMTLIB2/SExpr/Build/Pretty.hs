{-# LANGUAGE OverloadedStrings #-}

module BV.SMTLIB2.SExpr.Build.Pretty
    ( buildGenericSExprPretty
    , buildSExprPretty
    ) where

import BV.SMTLIB2.SExpr
import BV.SMTLIB2.SExpr.Build

import Data.Text.Lazy.Builder (Builder, fromString, singleton)
import Data.Text.Lazy.Builder.Int (decimal)
import GHC.Generics (Generic)

data PrettySExprConfig
  = PrettySExprConfig
      { indent :: Integer
      , preferredMaxLineWidth :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

buildGenericSExprPretty :: (a -> Builder) -> PrettySExprConfig -> GenericSExpr a -> Builder
buildGenericSExprPretty f = undefined

buildSExprPretty :: PrettySExprConfig -> SExpr -> Builder
buildSExprPretty = buildGenericSExprPretty buildAtom
