{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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
    , tryBinaryAtom
    , tryHexadecimalAtom
    , tryKeywordAtom
    , tryStringAtom
    , trySymbolAtom
      --
    , isValidBinaryAtom
    , isValidBinaryAtomChar
    , isValidHexadecimalAtom
    , isValidHexadecimalAtomChar
    , isValidKeywordAtom
    , isValidKeywordAtomChar
    , isValidStringAtomChar
    , isValidSymbolAtom
    , isValidSymbolAtomFirstChar
    , isValidSymbolAtomSubsequentChar
    ) where

import Control.DeepSeq (NFData)
import Data.Char (isDigit, isHexDigit, isLetter, isPrint, isSpace)
import Data.Maybe (fromJust)
import Data.String (IsString, fromString)
import Data.Traversable (foldMapDefault)
import GHC.Generics (Generic)
import GHC.IsList (IsList (..))
import GHC.Natural (Natural)

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
  = CheckedAtom UncheckedAtom
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

data UncheckedAtom
  = NumeralAtom Natural
  | HexadecimalAtom String
  | BinaryAtom String
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
        HexadecimalAtom s -> isValidHexadecimalAtom s
        BinaryAtom s -> isValidBinaryAtom s
        StringAtom s -> isValidStringAtom s
        SymbolAtom s -> isValidSymbolAtom s
        KeywordAtom s -> isValidKeywordAtom s
        _ -> True

unsafeAtom :: UncheckedAtom -> Atom
unsafeAtom = CheckedAtom

viewSExpr :: SExpr -> UncheckedSExpr
viewSExpr = fmap viewAtom

viewAtom :: Atom -> UncheckedAtom
viewAtom (CheckedAtom atom) = atom

numeralAtom :: Natural -> Atom
numeralAtom = unsafeAtom . NumeralAtom

tryHexadecimalAtom :: String -> Maybe Atom
tryHexadecimalAtom = checkAtom . HexadecimalAtom

hexadecimalAtom :: String -> Atom
hexadecimalAtom = fromJust . tryHexadecimalAtom

tryBinaryAtom :: String -> Maybe Atom
tryBinaryAtom = checkAtom . BinaryAtom

binaryAtom :: String -> Atom
binaryAtom = fromJust . tryBinaryAtom

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

isValidHexadecimalAtom :: String -> Bool
isValidHexadecimalAtom = all isValidHexadecimalAtomChar

isValidHexadecimalAtomChar :: Char -> Bool
isValidHexadecimalAtomChar = isHexDigit

isValidBinaryAtom :: String -> Bool
isValidBinaryAtom = all isValidBinaryAtomChar

isValidBinaryAtomChar :: Char -> Bool
isValidBinaryAtomChar c = c == '0' || c == '1'

isValidStringAtom :: String -> Bool
isValidStringAtom = all isValidStringAtomChar

isValidStringAtomChar :: Char -> Bool
isValidStringAtomChar c = isPrint c || isSpace c

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
isValidKeywordAtomChar c = isLetter c || isDigit c || c `elem` ("~!@$%^&*_-+=<>.?/" :: String)

instance IsString Atom where
    fromString = symbolAtom

instance IsString UncheckedAtom where
    fromString = SymbolAtom

instance IsString a => IsString (GenericSExpr a) where
    fromString = Atom . fromString

instance IsList (GenericSExpr a) where
    type Item (GenericSExpr a) = GenericSExpr a
    fromList = List
    toList (List xs) = xs
