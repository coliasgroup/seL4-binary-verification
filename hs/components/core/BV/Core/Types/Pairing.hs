module BV.Core.Types.Pairing
    ( Pairing (..)
    , PairingEq (..)
    , PairingEqDirection (..)
    , PairingEqSide (..)
    , PairingEqSideQuadrant (..)
    , PairingId (..)
    , PairingOf (..)
    , Pairings (..)
    , Tag (..)
    , asmIn
    , asmOut
    , cIn
    , cOut
    , prettyPairingEqDirection
    , prettyPairingEqSideQuadrant
    , prettyPairingId
    , prettyTag
    ) where

import qualified Data.Map as M
import GHC.Generics (Generic)

import BV.Core.Types.Program

data PairingOf a
  = PairingOf
      { c :: a
      , asm :: a
      }
  deriving (Eq, Generic, Ord, Show)

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

prettyPairingId :: PairingId -> String
prettyPairingId (PairingId { c, asm }) = asm.unwrapIdent ++ " (ASM)" ++ " <= " ++ c.unwrapIdent ++ " (C)"

prettyTag :: Tag ->  String
prettyTag C = "C"
prettyTag Asm = "ASM"

prettyPairingEqSideQuadrant :: PairingEqSideQuadrant ->  String
prettyPairingEqSideQuadrant (PairingEqSideQuadrant { tag, direction }) =
    prettyTag tag <> "_" <> prettyPairingEqDirection direction

prettyPairingEqDirection :: PairingEqDirection ->  String
prettyPairingEqDirection PairingEqDirectionIn = "IN"
prettyPairingEqDirection PairingEqDirectionOut = "OUT"
