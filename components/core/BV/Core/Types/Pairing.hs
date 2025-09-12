{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.Pairing
    ( Pairing (..)
    , PairingEq (..)
    , PairingEqDirection (..)
    , PairingEqSide (..)
    , PairingEqSideQuadrant (..)
    , PairingId
    , prettyPairingEqDirection
    , prettyPairingEqSideQuadrant
    , prettyPairingId
    , prettyTag
    ) where

import BV.Core.Types.Program
import BV.Core.Types.Tag

import Control.DeepSeq (NFData)
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
      , expr :: GraphExpr
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

prettyPairingId :: forall t. RefineTag t => PairingId t -> String
prettyPairingId pairingId =
    printf "%P (%P) <= %P (%P)"
        (pairingId ^. atTag leftTag)
        (leftTag :: t)
        (pairingId ^. atTag rightTag)
        (rightTag :: t)

prettyPairingEqSideQuadrant :: RefineTag t => PairingEqSideQuadrant t ->  String
prettyPairingEqSideQuadrant (PairingEqSideQuadrant { tag, direction }) =
    prettyTag tag <> "_" <> prettyPairingEqDirection direction

prettyPairingEqDirection :: PairingEqDirection ->  String
prettyPairingEqDirection PairingEqDirectionIn = "IN"
prettyPairingEqDirection PairingEqDirectionOut = "OUT"
