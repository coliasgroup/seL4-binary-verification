{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.SMTProofChecks
    ( AtomOrPlaceholder
    , FlattenedSMTProofChecks (..)
    , SExprPlaceholder (..)
    , SExprWithPlaceholders
    , SMTProofCheck (..)
    , SMTProofCheckGroup (..)
    , SMTProofCheckImp (..)
    , SMTProofChecks (..)
    , flattenSMTProofChecks
    , readSExprWithPlaceholders
    , readSExprsWithPlaceholders
    , splitSMTProofCheckGroup
    , tryReadSExprWithPlaceholders
    , tryReadSExprsWithPlaceholders
    ) where

import Control.Applicative (many, (<|>))
import Control.DeepSeq (NFData)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import qualified Text.ParserCombinators.ReadP as R

import BV.SMTLIB2.Types.SExpr
import BV.SMTLIB2.Types.SExpr.Read (anySExprWhitespaceP, atomP, genericSExprP)

import BV.Core.Types.Pairing
import BV.Core.Types.ProofScript

splitSMTProofCheckGroup :: SMTProofCheckGroup a -> [SMTProofCheck a]
splitSMTProofCheckGroup group = group.imps <&> \imp -> SMTProofCheck
    { setup = group.setup
    , imp
    }

newtype SMTProofChecks a
  = SMTProofChecks { unwrap :: M.Map PairingId (ProofScript [SMTProofCheckGroup a]) }
  deriving (Eq, Functor, Generic, Ord, Show)
  deriving newtype (NFData)

flattenSMTProofChecks :: SMTProofChecks a -> FlattenedSMTProofChecks a
flattenSMTProofChecks (SMTProofChecks byPairing) = FlattenedSMTProofChecks (M.map fold byPairing)

newtype FlattenedSMTProofChecks a
  = FlattenedSMTProofChecks { unwrap :: M.Map PairingId [SMTProofCheckGroup a] }
  deriving (Eq, Functor, Generic, Ord, Show)
  deriving newtype (NFData)

data SMTProofCheckGroup a
  = SMTProofCheckGroup
      { setup :: [SExprWithPlaceholders]
      , imps :: [SMTProofCheckImp a]
      }
  deriving (Eq, Functor, Generic, NFData, Ord, Show)

data SMTProofCheck a
  = SMTProofCheck
      { setup :: [SExprWithPlaceholders]
      , imp :: SMTProofCheckImp a
      }
  deriving (Eq, Functor, Generic, NFData, Ord, Show)

data SMTProofCheckImp a
  = SMTProofCheckImp
      { meta :: a
      , term :: SExprWithPlaceholders
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

type SExprWithPlaceholders = GenericSExpr AtomOrPlaceholder

type AtomOrPlaceholder = Either SExprPlaceholder Atom

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
atomOrPlaceholderP = Left <$> placeholderP <|> Right <$> atomP

placeholderP :: R.ReadP SExprPlaceholder
placeholderP = R.between (R.char '{') (R.char '}') $
    SExprPlaceholderMemSort <$ R.string "MemSort" <|> SExprPlaceholderMemDomSort <$ R.string "MemDomSort"
