module BV.Pairing
    ( Pairing (..)
    , PairingEq (..)
    , PairingEqDirection (..)
    , PairingEqSide (..)
    , PairingEqSideQuadrant (..)
    , PairingId (..)
    , Pairings (..)
    , Tag (..)
    , asmIn
    , asmOut
    , cIn
    , cOut
    ) where

import qualified Data.Map as M
import GHC.Generics (Generic)

import BV.Program

data Tag
  = C
  | Asm
  deriving (Eq, Generic, Ord, Show)

data PairingId
  = PairingId
      { c :: Ident
      , asm :: Ident
      }
  deriving (Eq, Generic, Ord, Show)

data Pairing
  = Pairing
      { inEqs :: [PairingEq]
      , outEqs :: [PairingEq]
      }
  deriving (Eq, Generic, Ord, Show)

data PairingEq
  = PairingEq
      { lhs :: PairingEqSide
      , rhs :: PairingEqSide
      }
  deriving (Eq, Generic, Ord, Show)

data PairingEqSide
  = PairingEqSide
      { quadrant :: PairingEqSideQuadrant
      , expr :: Expr
      }
  deriving (Eq, Generic, Ord, Show)

data PairingEqSideQuadrant
  = PairingEqSideQuadrant
      { tag :: Tag
      , direction :: PairingEqDirection
      }
  deriving (Eq, Generic, Ord, Show)

data PairingEqDirection
  = PairingEqDirectionIn
  | PairingEqDirectionOut
  deriving (Eq, Generic, Ord, Show)

asmIn :: PairingEqSideQuadrant
asmIn = PairingEqSideQuadrant Asm PairingEqDirectionIn

asmOut :: PairingEqSideQuadrant
asmOut = PairingEqSideQuadrant Asm PairingEqDirectionOut

cIn :: PairingEqSideQuadrant
cIn = PairingEqSideQuadrant C PairingEqDirectionIn

cOut :: PairingEqSideQuadrant
cOut = PairingEqSideQuadrant C PairingEqDirectionOut

newtype Pairings
  = Pairings (M.Map PairingId Pairing)
  deriving (Eq, Generic, Ord, Show)

--

