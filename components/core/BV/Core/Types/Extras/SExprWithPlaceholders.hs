{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.Core.Types.Extras.SExprWithPlaceholders where

import BV.Core.Arch
import BV.Core.Types.SExprWithPlaceholders
import BV.Core.Utils
import BV.SMTLIB2

import Data.Function (applyWhen)
import Data.Maybe (fromJust)
import Text.Printf (printf)

-- TODO private
type S = SExprWithPlaceholders

--

showSExprWithPlaceholders :: SExprWithPlaceholders -> String
showSExprWithPlaceholders = showGenericSExpr showAtomOrPlaceholder

showAtomOrPlaceholder :: AtomOrPlaceholder -> String
showAtomOrPlaceholder = \case
    AtomOrPlaceholderAtom atom -> showAtom atom
    AtomOrPlaceholderPlaceholder placeholder -> showSExprPlaceholder placeholder

showSExprPlaceholder :: SExprPlaceholder -> String
showSExprPlaceholder placeholder = "{" <> inner <> "}"
  where
    inner = case placeholder of
        SExprPlaceholderMemSort -> "MemSort"
        SExprPlaceholderMemDomSort -> "MemDomSort"

--

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
intS = intWithS negateS (atomS . numeralAtom. fromIntegral)

intWithS :: Integral a => (S -> S) -> (a -> S) -> a -> S
intWithS toNeg toAbs n = applyWhen (n /= nAbs) toNeg (toAbs nAbs)
  where
    nAbs = abs n

negateS :: S -> S
negateS x = ["-", x]

bvnegS :: S -> S
bvnegS x = ["bvneg", x]

defineFunS :: String -> [(String, S)] -> S -> S -> S
defineFunS name args ret body =
    [ "define-fun"
    , symbolS name
    , List (map (\(n, ty) -> List [symbolS n, ty]) args)
    , ret
    , body
    ]

declareFunS :: String -> [(String, S)] -> S -> S
declareFunS name args ret =
    [ "declare-fun"
    , symbolS name
    , List (map (\(n, ty) -> List [symbolS n, ty]) args)
    , ret
    ]

assertS :: S -> S
assertS fact = ["assert", fact]

bitVecS :: Integer -> S
bitVecS n = ["_", "BitVec", intS n]

boolS :: S
boolS = "Bool"

trueS :: S
trueS = "true"

falseS :: S
falseS = "false"

binOpS :: String -> S -> S -> S
binOpS op x y = [symbolS op, x, y]

bvaddS :: S -> S -> S
bvaddS = binOpS "bvadd"

notS :: S -> S
notS x = ["not", x]

andS :: S -> S -> S
andS = binOpS "and"

andNS :: [S] -> S
andNS xs = List $ "and" : xs

orS :: S -> S -> S
orS = binOpS "or"

orNS :: [S] -> S
orNS xs = List $ "or" : xs

iteS :: S -> S -> S -> S
iteS i t e = ["ite", i, t, e]

eqS :: S -> S -> S
eqS = binOpS "="

bvuleS :: S -> S -> S
bvuleS = binOpS "bvule"

bvandS :: S -> S -> S
bvandS = binOpS "bvand"

impliesS :: S -> S -> S
impliesS = binOpS "=>"

hexS :: String -> S
hexS = atomS . hexadecimalAtom

binS :: String -> S
binS = atomS . binaryAtom

intWithWidthS :: Integer -> Integer -> S
intWithWidthS bits = intWithS bvnegS $ \nAbs ->
    ensure (bits > 0 && nAbs >= 0 && nAbs < 2^bits) $
        f (printf ("%0." ++ show cols ++ fmt) nAbs)
  where
    (f, cols, fmt) = case bits `quotRem` 4 of
        (q, 0) -> (hexS, q, "x")
        _ -> (binS, bits, "b")

machineWordS :: Integer -> S
machineWordS = intWithWidthS archWordSizeBits

loadWord8S :: S -> S -> S
loadWord8S mem addr = ["load-word8", mem, addr]

loadWord32S :: S -> S -> S
loadWord32S mem addr = ["load-word32", mem, addr]

loadWord64S :: S -> S -> S
loadWord64S mem addr = ["load-word64", mem, addr]

storeWord8S :: S -> S -> S -> S
storeWord8S mem addr v = ["store-word8", mem, addr, v]

storeWord32S :: S -> S -> S -> S
storeWord32S mem addr v = ["store-word32", mem, addr, v]

storeWord64S :: S -> S -> S -> S
storeWord64S mem addr v = ["store-word64", mem, addr, v]

concatS :: S -> S -> S
concatS = binOpS "concat"

ixS :: String -> [S] -> S
ixS s ixs = listS $ ["_", symbolS s] ++ ixs

labelS :: String -> S -> S
labelS label x = ["!", x, keywordS "named", symbolS label]

--

parseSymbolS :: S -> Maybe String
parseSymbolS sexpr = do
    Atom (AtomOrPlaceholderAtom atom) <- return sexpr
    case viewAtom atom of
        SymbolAtom s -> Just s
        _ -> Nothing

matchPatternS :: Eq a => GenericSExpr a -> GenericSExpr a -> Bool
matchPatternS (Atom p) (Atom x) = p == x
matchPatternS (List ps) (List xs) = length ps <= length xs && and (zipWith matchPatternS ps xs)
matchPatternS _ _ = False
