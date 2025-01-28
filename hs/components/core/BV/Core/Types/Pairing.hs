{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.Pairing
    ( AtPairingId (..)
    , Pairing (..)
    , PairingEq (..)
    , PairingEqDirection (..)
    , PairingEqSide (..)
    , PairingEqSideQuadrant (..)
    , PairingId
    , PairingOf (..)
    , Pairings (..)
    , Tag (..)
    , WithTag (..)
    , intoPairingSide
    , pairingSide
    , pairingSideWithTag
    , prettyPairingEqDirection
    , prettyPairingEqSideQuadrant
    , prettyPairingId
    , prettyTag
    , withTag
    , withTags
    ) where

import BV.Core.Types.Program

import Control.DeepSeq (NFData)
import qualified Data.Map as M
import Data.Traversable (foldMapDefault)
import GHC.Generics (Generic)
import Optics.Core (Lens', view)

data PairingOf a
  = PairingOf
      { c :: a
      , asm :: a
      }
  deriving (Eq, Functor, Generic, NFData, Ord, Show)

instance Semigroup a => Semigroup (PairingOf a) where
    x <> y = PairingOf
        { c = x.c <> y.c
        , asm = x.asm <> y.asm
        }

instance Monoid a => Monoid (PairingOf a) where
    mempty = PairingOf
        { c = mempty
        , asm = mempty
        }

instance Applicative PairingOf where
    pure x = PairingOf x x
    ff <*> fx = PairingOf
        { c = ff.c fx.c
        , asm = ff.asm fx.asm
        }

instance Foldable PairingOf where
    foldMap = foldMapDefault

instance Traversable PairingOf where
    traverse f p = (\c asm -> PairingOf { c, asm }) <$> f p.c <*> f p.asm

data Tag
  = C
  | Asm
  deriving (Eq, Generic, NFData, Ord, Show)

data WithTag a
  = WithTag
      { tag :: Tag
      , value :: a
      }
  deriving (Eq, Functor, Generic, NFData, Ord, Show)

withTag :: (Tag -> a -> b) -> WithTag a -> b
withTag f (WithTag tag value) = f tag value

withTags :: PairingOf a -> PairingOf (WithTag a)
withTags pairing = PairingOf
    { asm = WithTag Asm pairing.asm
    , c = WithTag C pairing.c
    }

pairingSide :: Tag -> PairingOf a -> a
pairingSide tag = view (intoPairingSide tag)

pairingSideWithTag :: Tag -> PairingOf a -> WithTag a
pairingSideWithTag tag = WithTag tag . pairingSide tag

intoPairingSide :: Tag -> Lens' (PairingOf a) a
intoPairingSide C = #c
intoPairingSide Asm = #asm

type PairingId = PairingOf Ident

class AtPairingId a m | m -> a where
    atPairingId :: m -> PairingId -> a

instance AtPairingId a (M.Map PairingId a) where
    atPairingId = (M.!)

data Pairing
  = Pairing
      { inEqs :: [PairingEq]
      , outEqs :: [PairingEq]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PairingEq
  = PairingEq
      { lhs :: PairingEqSide
      , rhs :: PairingEqSide
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PairingEqSide
  = PairingEqSide
      { quadrant :: PairingEqSideQuadrant
      , expr :: Expr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PairingEqSideQuadrant
  = PairingEqSideQuadrant
      { tag :: Tag
      , direction :: PairingEqDirection
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PairingEqDirection
  = PairingEqDirectionIn
  | PairingEqDirectionOut
  deriving (Eq, Generic, NFData, Ord, Show)

newtype Pairings
  = Pairings { unwrap :: M.Map PairingId Pairing }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance Semigroup Pairings where
    x <> y = Pairings (x.unwrap <> y.unwrap)

instance Monoid Pairings where
    mempty = Pairings mempty

instance AtPairingId Pairing Pairings where
    atPairingId = atPairingId . (.unwrap)

prettyPairingId :: PairingId -> String
prettyPairingId (PairingOf { c, asm }) = asm.unwrap++ " (ASM)" ++ " <= " ++ c.unwrap ++ " (C)"

prettyTag :: Tag ->  String
prettyTag C = "C"
prettyTag Asm = "ASM"

prettyPairingEqSideQuadrant :: PairingEqSideQuadrant ->  String
prettyPairingEqSideQuadrant (PairingEqSideQuadrant { tag, direction }) =
    prettyTag tag <> "_" <> prettyPairingEqDirection direction

prettyPairingEqDirection :: PairingEqDirection ->  String
prettyPairingEqDirection PairingEqDirectionIn = "IN"
prettyPairingEqDirection PairingEqDirectionOut = "OUT"
