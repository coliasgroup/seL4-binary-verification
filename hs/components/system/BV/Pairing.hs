{-# LANGUAGE OverloadedStrings #-}

module BV.Pairing
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
import Data.Either (partitionEithers)
import Data.Functor (void)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (indentBlock)

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

instance ParseFile Pairings where
    parseFile =
        Pairings . M.fromList
            <$> parseBlocksFileWithTypicalKeyFormat ["Pairing"] parsePrettyPairingId parseInBlock

instance BuildToFile Pairings where
    buildToFile (Pairings pairings) =
        buildBlocksFileWithTypicalKeyFormat
            ["Pairing"]
            (fromString . prettyPairingId)
            buildInBlock
            (M.toList pairings)

instance ParseInBlock Pairing where
    parseInBlock = do
        line $ inLineSymbol "Pairing"
        (inEqs, outEqs) <- partitionEqs <$> manyTill eqLine (try endLine)
        return $ Pairing { inEqs, outEqs }
      where
        eqLine = line $ (,) <$> parseInLine <*> parseInLine
        endLine = line $ inLineSymbol "EndPairing"
        partitionEqs :: [(PairingEqDirection, PairingEq)] -> ([PairingEq], [PairingEq])
        partitionEqs = partitionEithers . map (\(direction, eq) -> eq & (case direction of
            PairingEqDirectionIn -> Left
            PairingEqDirectionOut -> Right))

instance BuildInBlock Pairing where
    buildInBlock pairing =
           lineInBlock "Pairing"
        <> mconcat (map (eqLine PairingEqDirectionIn) pairing.inEqs)
        <> mconcat (map (eqLine PairingEqDirectionOut) pairing.outEqs)
        <> lineInBlock "EndPairing"
      where
        eqLine direction eq = lineInBlock $ put direction <> put eq

instance ParseInLine PairingEq where
    parseInLine = PairingEq <$> parseInLine <*> parseInLine

instance BuildInLine PairingEq where
    buildInLine eq = put eq.lhs <> put eq.rhs

instance ParseInLine PairingEqSide where
    parseInLine = PairingEqSide <$> parseInLine <*> parseInLine

instance BuildInLine PairingEqSide where
    buildInLine side = put side.quadrant <> put side.expr

instance ParseInLine Tag where
    parseInLine = wordWithOr "invalid tag" $ \case
        "C" -> Just C
        "ASM" -> Just Asm
        _ -> Nothing

instance BuildInLine Tag where
    buildInLine = putWord . prettyTag

prettyTag :: Tag ->  String
prettyTag C = "C"
prettyTag Asm = "ASM"

instance ParseInLine PairingEqSideQuadrant where
    parseInLine = wordWithOr "invalid pairing eq side quadrant" $ \case
        "ASM_IN" -> Just asmIn
        "ASM_OUT" -> Just asmOut
        "C_IN" -> Just cIn
        "C_OUT" -> Just cOut
        _ -> Nothing

instance BuildInLine PairingEqSideQuadrant where
    buildInLine = putWord . prettyPairingEqSideQuadrant

prettyPairingEqSideQuadrant :: PairingEqSideQuadrant ->  String
prettyPairingEqSideQuadrant (PairingEqSideQuadrant { tag, direction }) =
    prettyTag tag <> "_" <> prettyPairingEqDirection direction

instance ParseInLine PairingEqDirection where
    parseInLine = wordWithOr "invalid pairing eq direction" $ \case
        "IN" -> Just PairingEqDirectionIn
        "OUT" -> Just PairingEqDirectionOut
        _ -> Nothing

instance BuildInLine PairingEqDirection where
    buildInLine = putWord . prettyPairingEqDirection

prettyPairingEqDirection :: PairingEqDirection ->  String
prettyPairingEqDirection PairingEqDirectionIn = "IN"
prettyPairingEqDirection PairingEqDirectionOut = "OUT"
