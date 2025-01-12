{-# LANGUAGE DeriveAnyClass #-}

module BV.SMTLIB2.Types.SExpr
    ( Atom
    , GenericSExpr (..)
    , SExpr
    , UncheckedAtom (..)
    , UncheckedSExpr
      --
    , checkAtom
    , checkSExpr
    , unsafeAtom
    , viewAtom
    , viewSExpr
      --
    , binaryAtom
    , hexadecimalAtom
    , keywordAtom
    , numeralAtom
    , stringAtom
    , symbolAtom
    , tryKeywordAtom
    , tryStringAtom
    , trySymbolAtom
      --
    , isValidKeywordAtom
    , isValidKeywordAtomChar
    , isValidStringAtomChar
    , isValidSymbolAtom
    , isValidSymbolAtomFirstChar
    , isValidSymbolAtomSubsequentChar
      --
    , showGenericSExpr
    , showSExpr
    , showUncheckedSExpr
    , showsAtom
    , showsGenericSExpr
    , showsSExpr
    , showsUncheckedAtom
    , showsUncheckedSExpr
    ) where

import Control.DeepSeq (NFData)
import Data.Char (isAscii, isDigit, isLetter, isPrint)
import Data.Maybe (fromJust)
import Data.Monoid (Endo (Endo, appEndo))
import Data.Traversable (foldMapDefault)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Numeric (showBin, showHex, showInt)

data GenericSExpr a
  = Atom a
  | List [GenericSExpr a]
  deriving (Eq, Functor, Generic, NFData, Ord, Show)

instance Applicative GenericSExpr where
    pure = Atom
    ff <*> fx = do
        f <- ff
        x <- fx
        return $ f x

instance Foldable GenericSExpr where
    foldMap = foldMapDefault

instance Traversable GenericSExpr where
    traverse f = \case
        Atom a -> Atom <$> f a
        List xs -> List <$> traverse (traverse f) xs

instance Monad GenericSExpr where
    ma >>= f = case ma of
        Atom a -> f a
        List xs -> List (map (>>= f) xs)

type SExpr = GenericSExpr Atom

type UncheckedSExpr = GenericSExpr UncheckedAtom

newtype Atom
  = CheckedAtom { unwrap :: UncheckedAtom }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

data UncheckedAtom
  = NumeralAtom Natural
  | HexadecimalAtom Natural
  | BinaryAtom Natural
  | StringAtom String
  | SymbolAtom String
  | KeywordAtom String
  deriving (Eq, Generic, NFData, Ord, Show)

checkSExpr :: UncheckedSExpr -> Maybe SExpr
checkSExpr = traverse checkAtom

checkAtom :: UncheckedAtom -> Maybe Atom
checkAtom unchecked = if ok then Just (CheckedAtom unchecked) else Nothing
  where
    ok = case unchecked of
        StringAtom s -> isValidStringAtom s
        SymbolAtom s -> isValidSymbolAtom s
        KeywordAtom s -> isValidKeywordAtom s
        _ -> True

unsafeAtom :: UncheckedAtom -> Atom
unsafeAtom = CheckedAtom

viewSExpr :: SExpr -> UncheckedSExpr
viewSExpr = fmap viewAtom

viewAtom :: Atom -> UncheckedAtom
viewAtom = (.unwrap)

numeralAtom :: Natural -> Atom
numeralAtom = unsafeAtom . NumeralAtom

hexadecimalAtom :: Natural -> Atom
hexadecimalAtom = unsafeAtom . HexadecimalAtom

binaryAtom :: Natural -> Atom
binaryAtom = unsafeAtom . BinaryAtom

tryStringAtom :: String -> Maybe Atom
tryStringAtom = checkAtom . StringAtom

stringAtom :: String -> Atom
stringAtom = fromJust . tryStringAtom

trySymbolAtom :: String -> Maybe Atom
trySymbolAtom = checkAtom . SymbolAtom

symbolAtom :: String -> Atom
symbolAtom = fromJust . trySymbolAtom

tryKeywordAtom :: String -> Maybe Atom
tryKeywordAtom = checkAtom . KeywordAtom

keywordAtom :: String -> Atom
keywordAtom = fromJust . tryKeywordAtom

isValidStringAtom :: String -> Bool
isValidStringAtom = all isValidStringAtomChar

isValidStringAtomChar :: Char -> Bool
isValidStringAtomChar c = isAscii c && isPrint c

isValidSymbolAtom :: String -> Bool
isValidSymbolAtom = \case
    [] -> False
    (c:cs) -> isValidSymbolAtomFirstChar c && all isValidSymbolAtomSubsequentChar cs

isValidSymbolAtomFirstChar :: Char -> Bool
isValidSymbolAtomFirstChar c = isValidSymbolAtomSubsequentChar c && not (isDigit c)

isValidSymbolAtomSubsequentChar :: Char -> Bool
isValidSymbolAtomSubsequentChar = isValidKeywordAtomChar

isValidKeywordAtom :: String -> Bool
isValidKeywordAtom s = not (null s) && all isValidKeywordAtomChar s

isValidKeywordAtomChar :: Char -> Bool
isValidKeywordAtomChar c = isLetter c || isDigit c || c `elem` "~!@$%^&*_-+=<>.?/"

showSExpr :: SExpr -> String
showSExpr = showGenericSExpr showsAtom

showsSExpr :: SExpr -> ShowS
showsSExpr = showsGenericSExpr showsAtom

showUncheckedSExpr :: UncheckedSExpr -> String
showUncheckedSExpr = showGenericSExpr showsUncheckedAtom

showsUncheckedSExpr :: UncheckedSExpr -> ShowS
showsUncheckedSExpr = showsGenericSExpr showsUncheckedAtom

showGenericSExpr :: (a -> ShowS) -> GenericSExpr a -> String
showGenericSExpr f = ($ "") . showsGenericSExpr f

showsGenericSExpr ::  (a -> ShowS) -> GenericSExpr a -> ShowS
showsGenericSExpr f = \case
    Atom a -> f a
    List [] -> showString "()"
    List (x:xs) ->
          showChar '('
        . showsGenericSExpr f x
        . foldr (\x' acc -> showChar ' ' . showsGenericSExpr f x' . acc) (showChar ')') xs

showsAtom :: Atom -> ShowS
showsAtom = showsUncheckedAtom . viewAtom

showsUncheckedAtom :: UncheckedAtom -> ShowS
showsUncheckedAtom = \case
    NumeralAtom n -> showInt n
    HexadecimalAtom n -> showString "#x" . showHex n
    BinaryAtom n -> showString "#b" . showBin n
    StringAtom s -> showChar '\"' . appEndo (mconcat (map (Endo . escapeChar) s)) . showChar '\"'
    SymbolAtom s -> showString s
    KeywordAtom s -> showString ":" . showString s
  where
    escapeChar = \case
        '"' -> showString "\\\""
        '\\' -> showString "\\\\"
        c -> showChar c
