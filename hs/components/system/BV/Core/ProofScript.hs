{-# LANGUAGE OverloadedStrings #-}

module BV.Core.ProofScript where

import Control.Applicative (many, optional, (<|>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.String (fromString)
import GHC.Generics (Generic)
import Optics.Core
import Text.Megaparsec (manyTill, manyTill_, try)

import BV.Core.Pairing
import BV.Core.Program
import BV.ConcreteSyntax.Parsing
import BV.ConcreteSyntax.Printing

data ProofNode
  = ProofNodeLeaf
  | ProofNodeRestr RestrProofNode
  | ProofNodeCaseSplit CaseSplitProofNode
  | ProofNodeSplit SplitProofNode
  | ProofNodeSingleRevInduct SingleRevInductProofNode
  deriving (Eq, Generic, Ord, Show)

data RestrProofNode
  = RestrProofNode
      { point :: NodeAddr
      , tag :: Tag
      , range :: RestrProofNodeRange
      , child :: ProofNode
      }
  deriving (Eq, Generic, Ord, Show)

data RestrProofNodeRange
  = RestrProofNodeRange
      { kind :: RestrProofNodeRangeKind
      , x :: Integer
      , y :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

data RestrProofNodeRangeKind
  = RestrProofNodeRangeKindNumber
  | RestrProofNodeRangeKindOffset
  deriving (Eq, Generic, Ord, Show)

data CaseSplitProofNode
  = CaseSplitProofNode
      { addr :: NodeAddr
      , tag :: Tag
      , left :: ProofNode
      , right :: ProofNode
      }
  deriving (Eq, Generic, Ord, Show)

data SplitProofNode
  = SplitProofNode
      { addr :: NodeAddr
      , loopRMax :: Integer
      , rDetails :: SplitProofNodeDetails
      , lDetails :: SplitProofNodeDetails
      , eqs :: [(Lambda, Lambda)]
      , p1 :: ProofNode
      , p2 :: ProofNode
      }
  deriving (Eq, Generic, Ord, Show)

data SplitProofNodeDetails
  = SplitProofNodeDetails
      { split :: Integer
      , seqStart :: Integer
      , step :: Integer
      , eqs :: [Lambda]
      }
  deriving (Eq, Generic, Ord, Show)

data SingleRevInductProofNode
  = SingleRevInductProofNode
      { point :: NodeAddr
      , tag :: Tag
      , n :: Integer
      , egs :: [Lambda]
      , pred :: Expr
      , nBounds :: Integer
      , child :: ProofNode
      }
  deriving (Eq, Generic, Ord, Show)

data Lambda
  = Lambda
      { freeVar :: Ident
      , freeVarTy :: ExprType
      , expr :: Expr
      }
  deriving (Eq, Generic, Ord, Show)

--

instance ParseInLine ProofNode where
    parseInLine = word >>= \case
        "Leaf" -> return ProofNodeLeaf
        "Restr" -> ProofNodeRestr <$> parseInLine
        "CaseSplit" -> ProofNodeCaseSplit <$> parseInLine
        "Split" -> ProofNodeSplit <$> parseInLine
        "SingleRevInduct" -> ProofNodeSingleRevInduct <$> parseInLine
        _ -> fail "invalid proof node"

instance ParseInLine RestrProofNode where
    parseInLine =
        RestrProofNode
            <$> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine

instance ParseInLine RestrProofNodeRange where
    parseInLine =
        RestrProofNodeRange
            <$> parseInLine
            <*> parseInLine
            <*> parseInLine

instance ParseInLine RestrProofNodeRangeKind where
    parseInLine = wordWithOr "invalid restr proof node reange kind" $ \case
        "Number" -> Just RestrProofNodeRangeKindNumber
        "Offset" -> Just RestrProofNodeRangeKindOffset
        _ -> Nothing

instance ParseInLine CaseSplitProofNode where
    parseInLine =
        CaseSplitProofNode
            <$> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine

instance ParseInLine SplitProofNode where
    parseInLine =
        SplitProofNode
            <$> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine

instance ParseInLine SplitProofNodeDetails where
    parseInLine =
        SplitProofNodeDetails
            <$> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine

instance ParseInLine SingleRevInductProofNode where
    parseInLine =
        SingleRevInductProofNode
            <$> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine

instance ParseInLine Lambda where
    parseInLine = do
        inLineSymbol "Lambda"
        Lambda <$> parseInLine <*> parseInLine <*> parseInLine

--

instance BuildInLine ProofNode where
    buildInLine = \case
        ProofNodeLeaf -> "Leaf"
        ProofNodeRestr node-> "Restr" <> put node
        ProofNodeCaseSplit node-> "CaseSplit" <> put node
        ProofNodeSplit node -> "Split" <> put node
        ProofNodeSingleRevInduct node -> "SingleRevInduct" <> put node

instance BuildInLine RestrProofNode where
    buildInLine range =
           put range.point
        <> put range.tag
        <> put range.range
        <> put range.child

instance BuildInLine RestrProofNodeRange where
    buildInLine range =
           put range.kind
        <> putDec range.x
        <> putDec range.y

instance BuildInLine RestrProofNodeRangeKind where
    buildInLine RestrProofNodeRangeKindNumber = "Number"
    buildInLine RestrProofNodeRangeKindOffset = "Offset"

instance BuildInLine CaseSplitProofNode where
    buildInLine node =
           put node.addr
        <> put node.tag
        <> put node.left
        <> put node.right

instance BuildInLine SplitProofNode where
    buildInLine node =
           put node.addr
        <> putDec node.loopRMax
        <> put node.rDetails
        <> put node.lDetails
        <> put node.eqs
        <> put node.p1
        <> put node.p2

instance BuildInLine SplitProofNodeDetails where
    buildInLine details =
           putDec details.split
        <> putDec details.seqStart
        <> putDec details.step
        <> put details.eqs

instance BuildInLine SingleRevInductProofNode where
    buildInLine node =
           put node.point
        <> put node.tag
        <> putDec node.n
        <> put node.egs
        <> put node.pred
        <> putDec node.nBounds
        <> put node.child

instance BuildInLine Lambda where
    buildInLine (Lambda { freeVar, freeVarTy, expr }) =
        "Lambda" <> put freeVar <> put freeVarTy <> put expr
