{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.Pairing
    ( AtPairingId (..)
    , Pairing (..)
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

type PairingId = ByTag' Ident

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
      { tag :: Tag'
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
prettyPairingId (ByRefineTag { c, asm }) = asm.unwrap ++ " (ASM)" ++ " <= " ++ c.unwrap ++ " (C)"

prettyPairingEqSideQuadrant :: PairingEqSideQuadrant ->  String
prettyPairingEqSideQuadrant (PairingEqSideQuadrant { tag, direction }) =
    prettyTag tag <> "_" <> prettyPairingEqDirection direction

prettyPairingEqDirection :: PairingEqDirection ->  String
prettyPairingEqDirection PairingEqDirectionIn = "IN"
prettyPairingEqDirection PairingEqDirectionOut = "OUT"
