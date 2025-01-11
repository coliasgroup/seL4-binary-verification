{-# LANGUAGE DeriveAnyClass #-}

module BV.SMTLIB2.Types.SExpr
    ( SExpr
    , SExprConstant
    , SExprConstantView (..)
    , SExprShallowView (..)
    , SExprView (..)
    , binarySExpr
    , hexadecimalSExpr
    , keywordSExpr
    , listSExpr
    , numeralSExpr
    , showSExpr
    , showsSExpr
    , stringSExpr
    , symbolSExpr
    , tryKeywordSExpr
    , trySExprFromView
    , tryStringSExpr
    , trySymbolSExpr
    , viewSExpr
    , viewSExprConstant
    , viewSExprShallow
    ) where

import Control.DeepSeq (NFData)
import Data.Char
import Data.Maybe (fromJust)
import Data.Monoid (Endo (Endo, appEndo))
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Numeric (showBin, showHex, showInt)

data SExprConstant
  = InternalSExprConstantNumeral Natural
  | InternalSExprConstantHexadecimal Natural
  | InternalSExprConstantBinary Natural
  | InternalSExprConstantString String
  deriving (Eq, Generic, NFData, Ord, Show)

data SExpr
  = InternalSExprConstant SExprConstant
  | InternalSExprSymbol String
  | InternalSExprKeyword String
  | InternalSExprList [SExpr]
  deriving (Eq, Generic, NFData, Ord, Show)

numeralSExpr :: Natural -> SExpr
numeralSExpr = InternalSExprConstant . InternalSExprConstantNumeral

hexadecimalSExpr :: Natural -> SExpr
hexadecimalSExpr = InternalSExprConstant . InternalSExprConstantHexadecimal

binarySExpr :: Natural -> SExpr
binarySExpr = InternalSExprConstant . InternalSExprConstantBinary

tryStringSExpr :: String -> Maybe SExpr
tryStringSExpr = (InternalSExprConstant . InternalSExprConstantString) `checking` isValidSExprString

stringSExpr :: String -> SExpr
stringSExpr = fromJust . tryStringSExpr

trySymbolSExpr :: String -> Maybe SExpr
trySymbolSExpr = InternalSExprSymbol `checking` isValidSExprSymbol

symbolSExpr :: String -> SExpr
symbolSExpr = fromJust . trySymbolSExpr

tryKeywordSExpr :: String -> Maybe SExpr
tryKeywordSExpr = InternalSExprKeyword `checking` isValidSExprKeyword

keywordSExpr :: String -> SExpr
keywordSExpr = fromJust . tryKeywordSExpr

listSExpr :: [SExpr] -> SExpr
listSExpr = InternalSExprList

checking :: (a -> b) -> (a -> Bool) -> a -> Maybe b
checking f p x = if p x then Just (f x) else Nothing

isValidSExprString :: String -> Bool
isValidSExprString = all $ \c -> isAscii c && isPrint c

isValidSExprSymbol :: String -> Bool
isValidSExprSymbol s = isValidSExprKeyword s && case s of
    (c:_) -> not (isDigit c)
    _ -> True

isValidSExprKeyword :: String -> Bool
isValidSExprKeyword = all p
  where
    p c = isLetter c || isDigit c || c `elem` ("~!@$%^&*_-+=<>.?/" :: String)

data SExprConstantView
  = SExprConstantNumeral Natural
  | SExprConstantHexadecimal Natural
  | SExprConstantBinary Natural
  | SExprConstantString String
  deriving (Eq, Generic, NFData, Ord, Show)

data SExprShallowView
  = SExprShallowConstant SExprConstant
  | SExprShallowSymbol String
  | SExprShallowKeyword String
  | SExprShallowList [SExpr]
  deriving (Eq, Generic, NFData, Ord, Show)

data SExprView
  = SExprConstant SExprConstantView
  | SExprSymbol String
  | SExprKeyword String
  | SExprList [SExprView]
  deriving (Eq, Generic, NFData, Ord, Show)

viewSExprShallow :: SExpr -> SExprShallowView
viewSExprShallow = \case
    InternalSExprConstant c -> SExprShallowConstant c
    InternalSExprSymbol s -> SExprShallowSymbol s
    InternalSExprKeyword s -> SExprShallowKeyword s
    InternalSExprList xs -> SExprShallowList xs

viewSExpr :: SExpr -> SExprView
viewSExpr = \case
    InternalSExprConstant c -> SExprConstant (viewSExprConstant c)
    InternalSExprSymbol s -> SExprSymbol s
    InternalSExprKeyword s -> SExprKeyword s
    InternalSExprList xs -> SExprList (map viewSExpr xs)

viewSExprConstant :: SExprConstant -> SExprConstantView
viewSExprConstant = \case
    InternalSExprConstantNumeral n -> SExprConstantNumeral n
    InternalSExprConstantHexadecimal n -> SExprConstantHexadecimal n
    InternalSExprConstantBinary n -> SExprConstantBinary n
    InternalSExprConstantString s -> SExprConstantString s

trySExprFromView :: SExprView -> Maybe SExpr
trySExprFromView = \case
    SExprConstant c -> trySExprConstantFromView c
    SExprSymbol s -> trySymbolSExpr s
    SExprKeyword s -> tryKeywordSExpr s
    SExprList xs -> listSExpr <$> traverse trySExprFromView xs

trySExprConstantFromView :: SExprConstantView -> Maybe SExpr
trySExprConstantFromView = \case
    SExprConstantNumeral n -> return $ numeralSExpr n
    SExprConstantHexadecimal n -> return $ hexadecimalSExpr n
    SExprConstantBinary n -> return $ binarySExpr n
    SExprConstantString s -> tryStringSExpr s

showSExpr :: SExpr -> String
showSExpr = ($ "") . showsSExpr

showsSExpr :: SExpr -> ShowS
showsSExpr = \case
    InternalSExprConstant c -> showsSExprConstant c
    InternalSExprSymbol s -> showString s
    InternalSExprKeyword s -> showString ":" . showString s
    InternalSExprList [] -> showString "()"
    InternalSExprList (x:xs) ->
          showChar '('
        . showsSExpr x
        . foldr (\x' acc -> showChar ' ' . showsSExpr x' . acc) (showChar ')') xs

showsSExprConstant :: SExprConstant -> ShowS
showsSExprConstant = \case
    InternalSExprConstantNumeral n -> showInt n
    InternalSExprConstantHexadecimal n -> showString "#x" . showHex n
    InternalSExprConstantBinary n -> showString "#b" . showBin n
    InternalSExprConstantString s -> showChar '\"' . appEndo (mconcat (map (Endo . escapeChar) s)) . showChar '\"'
  where
    escapeChar '"' = showString "\\\""
    escapeChar '\\' = showString "\\\\"
    escapeChar c = showChar c
