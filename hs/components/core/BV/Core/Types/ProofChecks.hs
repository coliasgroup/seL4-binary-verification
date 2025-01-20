{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.ProofChecks
    ( AtomOrPlaceholder
    , EqHyp (..)
    , EqHypInduct (..)
    , EqHypSide (..)
    , FlattenedProofChecks (..)
    , FlattenedSMTProofChecks (..)
    , Hyp (..)
    , PcImpHyp (..)
    , PcImpHypSide (..)
    , ProofCheck (..)
    , ProofChecks (..)
    , Restr (..)
    , SExprPlaceholder (..)
    , SExprWithPlaceholders
    , SMTProofCheck (..)
    , SMTProofCheckGroup (..)
    , SMTProofCheckImp (..)
    , SMTProofChecks (..)
    , Visit (..)
    , VisitCount (..)
    , VisitWithTag (..)
    , flattenProofChecks
    , flattenSMTProofChecks
    , readSExprWithPlaceholders
    , readSExprsWithPlaceholders
    , tryReadSExprWithPlaceholders
    , tryReadSExprsWithPlaceholders
    ) where

import Control.Applicative (many, (<|>))
import Control.DeepSeq (NFData)
import Data.Foldable (fold)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import qualified Text.ParserCombinators.ReadP as R

import BV.SMTLIB2.Types.SExpr
import BV.SMTLIB2.Types.SExpr.Read (anySExprWhitespaceP, atomP, genericSExprP)

import BV.Core.Types.Pairing
import BV.Core.Types.Program
import BV.Core.Types.ProofScript

newtype ProofChecks a
  = ProofChecks { unwrap :: M.Map PairingId (ProofScript [ProofCheck a]) }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving newtype (NFData)

flattenProofChecks :: ProofChecks a -> FlattenedProofChecks a
flattenProofChecks (ProofChecks byPairing) = FlattenedProofChecks (M.map fold byPairing)

newtype FlattenedProofChecks a
  = FlattenedProofChecks { unwrap :: M.Map PairingId [ProofCheck a] }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving newtype (NFData)

instance AtPairingId (ProofScript [ProofCheck a]) (ProofChecks a) where
    atPairingId = atPairingId . (.unwrap)

data ProofCheck a
  = ProofCheck
      { meta :: a
      , hyps :: [Hyp]
      , hyp :: Hyp
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data Hyp
  = HypPcImp PcImpHyp
  | HypEq
      { ifAt :: Bool
      , eq :: EqHyp
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PcImpHyp
  = PcImpHyp
      { lhs :: PcImpHypSide
      , rhs :: PcImpHypSide
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PcImpHypSide
  = PcImpHypSideBool Bool
  | PcImpHypSidePc VisitWithTag
  deriving (Eq, Generic, NFData, Ord, Show)

data EqHyp
  = EqHyp
      { lhs :: EqHypSide
      , rhs :: EqHypSide
      , induct :: Maybe EqHypInduct
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data EqHypSide
  = EqHypSide
      { expr :: Expr
      , visit :: VisitWithTag
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data EqHypInduct
  = EqHypInduct
      { a :: Integer
      , b :: Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data VisitWithTag
  = VisitWithTag
      { visit :: Visit
      , tag :: Tag
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data Visit
  = Visit
      { nodeId :: NodeId
      , restrs :: [Restr]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data Restr
  = Restr
      { nodeId :: NodeId
      , visitCount :: VisitCount
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data VisitCount
  = VisitCount
      { numbers :: [Integer]
      , offsets :: [Integer]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Semigroup VisitCount where
    x <> y = VisitCount
        { numbers = x.numbers <> y.numbers
        , offsets = x.offsets <> y.offsets
        }

instance Monoid VisitCount where
    mempty = VisitCount [] []

--

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
