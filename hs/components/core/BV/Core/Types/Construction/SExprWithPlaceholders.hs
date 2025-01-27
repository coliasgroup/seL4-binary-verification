{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.Core.Types.Construction.SExprWithPlaceholders where

import BV.Core.Types.SExprWithPlaceholders
import BV.SMTLIB2.Types

import Data.Function (applyWhen)
import Data.Maybe (fromJust)

type S = SExprWithPlaceholders

listS :: [S] -> S
listS = List

atomS :: Atom -> S
atomS = Atom . AtomOrPlaceholderAtom

uncheckedAtomS :: UncheckedAtom -> S
uncheckedAtomS = atomS . fromJust . checkAtom

placeholderS :: SExprPlaceholder -> S
placeholderS = Atom . AtomOrPlaceholderPlaceholder

memSortS :: S
memSortS = placeholderS SExprPlaceholderMemSort

memDomSortS :: S
memDomSortS = placeholderS SExprPlaceholderMemDomSort

intS :: Integral a => a -> S
intS n = applyWhen (n /= nAbs) negateS (Atom (AtomOrPlaceholderAtom (numeralAtom (fromIntegral nAbs))))
  where
    nAbs = abs n

negateS :: S -> S
negateS x = ["-", x]
