{-# LANGUAGE OverloadedStrings #-}

module BV.Core.Inputs where

import qualified Data.Map as M
import GHC.Generics (Generic)

import BV.Core.Pairing
import BV.Core.Problem
import BV.Core.Program
import BV.Core.ProofScript
import BV.System.Parsing
import BV.System.Printing
import Control.Applicative (many, optional)
import Data.String (fromString)

newtype StackBounds
  = StackBounds (M.Map Ident Expr)
  deriving (Eq, Generic, Ord, Show)

newtype InlineScripts
  = InlineScripts (M.Map PairingId [InlineScriptEntry])
  deriving (Eq, Generic, Ord, Show)

type InlineScript = [InlineScriptEntry]

data InlineScriptEntry
  = InlineScriptEntry
      { nodeBySource :: NodeBySource
      , inlinedFunctionName :: Ident
      }
  deriving (Eq, Generic, Ord, Show)

newtype ProblemsAndProofs
  = ProblemsAndProofs (M.Map PairingId ProblemAndProof)
  deriving (Eq, Generic, Ord, Show)

data ProblemAndProof
  = ProblemAndProof
      { problem :: Problem
      , proof :: ProofNode
      }
  deriving (Eq, Generic, Ord, Show)

--

instance ParseFile StackBounds where
    parseFile = do
        optional . line $ inLineSymbol "FunctionHash" *> parseInLine @Integer
        StackBounds . M.fromList <$> many p
      where
        p = line $ do
            inLineSymbol "StackBound"
            (,) <$> parseInLine <*> parseInLine

instance BuildToFile StackBounds where
    buildToFile (StackBounds stackBounds) = buildBlock $ mconcat (map f (M.toList stackBounds))
      where
        f (ident, expr) = lineInBlock $ "StackBound" <> put ident <> put expr

instance ParseFile ProblemsAndProofs where
    parseFile =
        ProblemsAndProofs . M.fromList
            <$> parseBlocksFileWithTypicalKeyFormat ["ProblemProof", "Problem", "Pairing"] parsePrettyPairingId parseInBlock


instance BuildToFile ProblemsAndProofs where
    buildToFile (ProblemsAndProofs problemsAndProofs) =
        buildBlocksFileWithTypicalKeyFormat
            ["ProblemProof", "Problem", "Pairing"]
            (fromString . prettyPairingId)
            buildInBlock
            (M.toList problemsAndProofs)

instance ParseInBlock ProblemAndProof where
    parseInBlock = ProblemAndProof <$> parseInBlock <*> line parseInLine

instance BuildInBlock ProblemAndProof where
    buildInBlock (ProblemAndProof { problem, proof }) = buildInBlock problem <> lineInBlock (put proof)
