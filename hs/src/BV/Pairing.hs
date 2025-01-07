{-# LANGUAGE OverloadedStrings #-}

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
    , parsePrettyPairingId
    , parsePythonPairingName
    , prettyPairingId
    ) where

import Control.Applicative (many, optional, (<|>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.String (fromString)
import GHC.Generics (Generic)
import Optics.Core
import Text.Megaparsec (manyTill, manyTill_, satisfy, some, try)

import BV.Parsing
import BV.Printing
import BV.Program
import BV.Utils
import Data.Char (isSpace)
import Data.Functor (void)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (indentBlock)

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

parsePrettyPairingId :: Parser PairingId
parsePrettyPairingId = do
    asm <- ident
    hspace *> "(ASM)" *> hspace *> "<=" *> hspace
    c <- ident
    hspace *> "(C)"
    return $ PairingId { asm, c }
  where
    ident = Ident <$> some (satisfy isIdentChar)
    isIdentChar c = not (isSpace c || c == '(' || c == ')')

parsePythonPairingName :: Parser PairingId
parsePythonPairingName = do
    "Pairing" *> hspace1 *> "(" *> hspace
    pairingId <- parsePrettyPairingId
    ")"
    return pairingId

--

instance ParseInLine Tag where
    parseInLine = wordWithOr "invalid tag" $ \case
        "C" -> Just C
        "ASM" -> Just Asm
        _ -> Nothing
