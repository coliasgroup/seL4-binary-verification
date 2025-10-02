{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.Core.Types.Extras.SExprToExpr
    ( sexprToExpr
    ) where

import BV.Core.Types
import BV.Core.Types.Extras.Expr
import BV.SMTLIB2

import Data.List (genericLength)
import Numeric (readBin, readHex)
import qualified Text.Read as P

sexprToExpr :: SExpr -> Expr c
sexprToExpr sexpr = case viewSExpr sexpr of
    Atom (SymbolAtom "true") -> trueE
    Atom (SymbolAtom "false") -> falseE
    Atom (BinaryAtom n) -> numE (wordT (genericLength n)) (runReadS readBin n)
    Atom (HexadecimalAtom n) -> numE (wordT (genericLength n * 4)) (runReadS readHex n)

runReadS :: P.ReadS a -> String -> a
runReadS p s = case filter (null . snd) (p s) of
    [(a, "")] -> a
