{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.Pairing
    ( Pairing (..)
    , PairingEq (..)
    , PairingEqDirection (..)
    , PairingEqSide (..)
    , PairingEqSideQuadrant (..)
    , PairingId
    , Pairings (..)
    , prettyPairingEqDirection
    , prettyPairingEqSideQuadrant
    , prettyPairingId
    , prettyTag
    ) where

import BV.Core.Types.Program
import BV.Core.Types.Tag

import Control.DeepSeq (NFData)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics
import Text.Printf (printf)

type PairingId t = ByTag t Ident

data Pairing t
  = Pairing
      { inEqs :: [PairingEq t]
      , outEqs :: [PairingEq t]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PairingEq t
  = PairingEq
      { lhs :: PairingEqSide t
      , rhs :: PairingEqSide t
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PairingEqSide t
  = PairingEqSide
      { quadrant :: PairingEqSideQuadrant t
      , expr :: Expr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PairingEqSideQuadrant t
  = PairingEqSideQuadrant
      { tag :: t
      , direction :: PairingEqDirection
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PairingEqDirection
  = PairingEqDirectionIn
  | PairingEqDirectionOut
  deriving (Eq, Generic, NFData, Ord, Show)

newtype Pairings t
  = Pairings { unwrap :: M.Map (PairingId t) (Pairing t) }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance Semigroup (Pairings t) where
    x <> y = Pairings (x.unwrap <> y.unwrap)

instance Monoid (Pairings t) where
    mempty = Pairings mempty

prettyPairingId :: forall t. RefineTag t => PairingId t -> String
prettyPairingId pairingId =
    printf "%s (%s) <= %s (%s)"
        (pairingId ^. atTag leftTag).unwrap
        (prettyTag (leftTag :: t))
        (pairingId ^. atTag rightTag).unwrap
        (prettyTag (rightTag :: t))

prettyPairingEqSideQuadrant :: RefineTag t => PairingEqSideQuadrant t ->  String
prettyPairingEqSideQuadrant (PairingEqSideQuadrant { tag, direction }) =
    prettyTag tag <> "_" <> prettyPairingEqDirection direction

prettyPairingEqDirection :: PairingEqDirection ->  String
prettyPairingEqDirection PairingEqDirectionIn = "IN"
prettyPairingEqDirection PairingEqDirectionOut = "OUT"
