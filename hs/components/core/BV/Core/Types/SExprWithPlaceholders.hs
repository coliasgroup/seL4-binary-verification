{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.SExprWithPlaceholders
    ( AtomOrPlaceholder (..)
    , SExprPlaceholder (..)
    , SExprWithPlaceholders
    , readSExprWithPlaceholders
    , readSExprsWithPlaceholders
    , tryReadSExprWithPlaceholders
    , tryReadSExprsWithPlaceholders
    ) where

import Control.Applicative (many, (<|>))
import Control.DeepSeq (NFData)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.String (IsString, fromString)
import GHC.Generics (Generic)
import qualified Text.ParserCombinators.ReadP as R

import BV.SMTLIB2.Types.SExpr
import BV.SMTLIB2.Types.SExpr.Read (anySExprWhitespaceP, atomP, genericSExprP)

type SExprWithPlaceholders = GenericSExpr AtomOrPlaceholder

data AtomOrPlaceholder
  = AtomOrPlaceholderAtom Atom
  | AtomOrPlaceholderPlaceholder SExprPlaceholder
  deriving (Eq, Generic, NFData, Ord, Show)

data SExprPlaceholder
  = SExprPlaceholderMemSort
  | SExprPlaceholderMemDomSort
  deriving (Eq, Generic, NFData, Ord, Show)

readSToTryRead :: ReadS a -> String -> Maybe a
readSToTryRead p s = case p s of
    [(a, "")] -> Just a
    [] -> Nothing
    _ -> error "unreachable"

readPToTryRead :: R.ReadP a -> String -> Maybe a
readPToTryRead = readSToTryRead . R.readP_to_S . (<* R.eof)

readSExprsWithPlaceholders :: String -> [SExprWithPlaceholders]
readSExprsWithPlaceholders = fromJust . tryReadSExprsWithPlaceholders

readSExprWithPlaceholders :: String -> SExprWithPlaceholders
readSExprWithPlaceholders = fromJust . tryReadSExprWithPlaceholders

tryReadSExprsWithPlaceholders :: String -> Maybe [SExprWithPlaceholders]
tryReadSExprsWithPlaceholders = readPToTryRead $ anySExprWhitespaceP *> many (sexprWithPlaceholdersP <* anySExprWhitespaceP)

tryReadSExprWithPlaceholders :: String -> Maybe SExprWithPlaceholders
tryReadSExprWithPlaceholders = readPToTryRead sexprWithPlaceholdersP

sexprWithPlaceholdersP :: R.ReadP SExprWithPlaceholders
sexprWithPlaceholdersP = genericSExprP atomOrPlaceholderP

atomOrPlaceholderP :: R.ReadP AtomOrPlaceholder
atomOrPlaceholderP = AtomOrPlaceholderAtom <$> atomP <|> AtomOrPlaceholderPlaceholder <$> placeholderP

placeholderP :: R.ReadP SExprPlaceholder
placeholderP = R.between (R.char '{') (R.char '}') $
    SExprPlaceholderMemSort <$ R.string "MemSort" <|> SExprPlaceholderMemDomSort <$ R.string "MemDomSort"

instance IsString AtomOrPlaceholder where
    fromString = AtomOrPlaceholderAtom . fromString
