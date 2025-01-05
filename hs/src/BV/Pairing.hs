module BV.Pairing where

import GHC.Generics (Generic)

import BV.Program

data Tag
  = C
  | Asm
  deriving (Generic, Show)

data PairingID
  = PairingID
      { c :: Ident
      , asm :: Ident
      }
  deriving (Generic, Show)
