{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.SExpr
    ( SExpr (..)
    , showSExpr
    , showsSExpr
    ) where

import Control.DeepSeq (NFData)
import Data.Char
import GHC.Generics (Generic)

data SExpr
  = Atom String
  | List [SExpr]
  deriving (Eq, Generic, NFData, Ord, Show)

-- showSExpr :: SExpr -> String
-- showSExpr = ($ "") . showsSExpr

showSExpr :: SExpr -> String
showSExpr = const ""

showsSExpr :: SExpr -> ShowS
showsSExpr = undefined $ \case
    Atom x -> showsAtom x
    List [] -> showString "()"
    List (x:xs) ->
          showChar '('
        . showsSExpr x
        . foldr (\x' acc -> showChar ' ' . showsSExpr x' . acc) (showChar ')') xs

showsAtom :: String -> ShowS
showsAtom s =
    if not (all isSupportedAtomChar s)
    then error $ "unsupported atom: " ++ show s
    else showString s

isSupportedAtomChar :: Char -> Bool
isSupportedAtomChar c = isPrint c && not (isSpace c) && c /= ')' && c /= '"' && c /= '|'
