{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.SExprWithPlaceholders
    ( AtomOrPlaceholder
    , GenericAtomOrPlaceholder (..)
    , SExprPlaceholder (..)
    , SExprWithPlaceholders
    , checkSExprWithPlaceholders
    , readSExprWithPlaceholders
    , readSExprsWithPlaceholders
    , tryReadSExprWithPlaceholders
    , tryReadSExprsWithPlaceholders
    , viewSExprWithPlaceholders
    ) where

import BV.SMTLIB2.Types.SExpr
import BV.SMTLIB2.Types.SExpr.Read (anySExprWhitespaceP, atomP, genericSExprP)

import Control.Applicative (many, (<|>))
import Control.DeepSeq (NFData)
import Data.Maybe (fromJust)
import Data.String (IsString, fromString)
import GHC.Generics (Generic)
import qualified Text.ParserCombinators.ReadP as R

type SExprWithPlaceholders = GenericSExpr AtomOrPlaceholder

data GenericAtomOrPlaceholder a
  = AtomOrPlaceholderAtom a
  | AtomOrPlaceholderPlaceholder SExprPlaceholder
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

type AtomOrPlaceholder = GenericAtomOrPlaceholder Atom

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

instance IsString a => IsString (GenericAtomOrPlaceholder a) where
    fromString = AtomOrPlaceholderAtom . fromString

viewSExprWithPlaceholders :: SExprWithPlaceholders -> GenericSExpr (GenericAtomOrPlaceholder UncheckedAtom)
viewSExprWithPlaceholders = fmap (fmap viewAtom)

checkSExprWithPlaceholders :: GenericSExpr (GenericAtomOrPlaceholder UncheckedAtom) -> Maybe SExprWithPlaceholders
checkSExprWithPlaceholders = traverse (traverse checkAtom)
