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
    , asmIn
    , asmInQ
    , asmOut
    , asmOutQ
    , cIn
    , cInQ
    , cOut
    , cOutQ
    , intoPairingSide
    , pairingSide
    , pairingSideWithTag
    , prettyPairingEqDirection
    , prettyPairingEqSideQuadrant
    , prettyPairingId
    , prettyTag
    , withTag
    , (===)
    ) where

import Control.DeepSeq (NFData)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics.Core (Lens', view)

import BV.Core.Types.Program

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

asmInQ :: PairingEqSideQuadrant
asmInQ = PairingEqSideQuadrant Asm PairingEqDirectionIn

asmOutQ :: PairingEqSideQuadrant
asmOutQ = PairingEqSideQuadrant Asm PairingEqDirectionOut

cInQ :: PairingEqSideQuadrant
cInQ = PairingEqSideQuadrant C PairingEqDirectionIn

cOutQ :: PairingEqSideQuadrant
cOutQ = PairingEqSideQuadrant C PairingEqDirectionOut

asmIn :: Expr -> PairingEqSide
asmIn = PairingEqSide asmInQ

asmOut :: Expr -> PairingEqSide
asmOut = PairingEqSide asmOutQ

cIn :: Expr -> PairingEqSide
cIn = PairingEqSide cInQ

cOut :: Expr -> PairingEqSide
cOut = PairingEqSide cOutQ

infix 4 ===

(===) :: PairingEqSide -> PairingEqSide -> PairingEq
(===) = PairingEq

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
