{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.Core.Types.Extras.SExprWithPlaceholders where

import BV.Core.Types.SExprWithPlaceholders
import BV.Core.Utils
import BV.SMTLIB2.Types

import Data.Function (applyWhen)
import Data.Maybe (fromJust)
import Text.Printf (printf)

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

symbolS :: String -> S
symbolS = atomS . symbolAtom

keywordS :: String -> S
keywordS = atomS . keywordAtom

intS :: Integral a => a -> S
intS = intWithS (atomS . numeralAtom. fromIntegral)

intWithS :: Integral a => (a -> S) -> a -> S
intWithS toAbs n = applyWhen (n /= nAbs) negateS (toAbs nAbs)
  where
    nAbs = abs n

negateS :: S -> S
negateS x = ["-", x]

defineFunS :: String -> [(String, S)] -> S -> S -> S
defineFunS name args ret body =
    [ "define-fun"
    , symbolS name
    , List (map (\(n, ty) -> List [symbolS n, ty]) args)
    , ret
    , body
    ]

bitVecS :: Integer -> S
bitVecS n = ["_", "BitVec", intS n]

binOpS :: String -> S -> S -> S
binOpS op x y = [symbolS op, x, y]

bvaddS :: S -> S -> S
bvaddS = binOpS "bvadd"

notS :: S -> S
notS x = ["not", x]

andNS :: [S] -> S
andNS xs = List $ "and" : xs

iteS :: S -> S -> S -> S
iteS i t e = ["ite", i, t, e]

eqS :: S -> S -> S
eqS = binOpS "="

hexS :: String -> S
hexS = atomS . hexadecimalAtom

binS :: String -> S
binS = atomS . binaryAtom

intWithWidthS :: Integer -> Integer -> S
intWithWidthS bits = intWithS $ \nAbs ->
    ensure (bits > 0 && nAbs < 2^bits) $
        f (printf ("%0." ++ show cols ++ fmt) nAbs)
  where
    (f, cols, fmt) = case bits `quotRem` 4 of
        (q, 0) -> (hexS, q, "x")
        _ -> (binS, bits, "b")

concatS :: S -> S -> S
concatS = binOpS "concat"

ixS :: String -> [S] -> S
ixS s ixs = listS $ ["_", symbolS s] ++ ixs

labelS :: String -> S -> S
labelS label x = ["!", x, keywordS "named", symbolS label]

--

matchPatternS :: Eq a => GenericSExpr a -> GenericSExpr a -> Bool
matchPatternS (Atom p) (Atom x) = p == x
matchPatternS (List ps) (List xs) = length ps <= length xs && and (zipWith matchPatternS ps xs)
matchPatternS _ _ = False
