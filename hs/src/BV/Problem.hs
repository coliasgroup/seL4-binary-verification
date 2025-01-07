{-# LANGUAGE OverloadedStrings #-}

module BV.Problem where

import Control.Applicative (many, optional, (<|>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.String (fromString)
import GHC.Generics (Generic)
import Optics.Core
import Text.Megaparsec (manyTill, manyTill_, try)

import BV.Pairing
import BV.Parsing
import BV.Printing
import BV.Program
import BV.Utils

data Problem
  = Problem
      { c :: ProblemSide
      , asm :: ProblemSide
      , nodes :: NodeMap
      }
  deriving (Eq, Generic, Ord, Show)

data ProblemSide
  = ProblemSide
      { name :: Ident
      , input :: [Argument]
      , output :: [Argument]
      , entryPoint :: NodeId
      }
  deriving (Eq, Generic, Ord, Show)

data NodeBySource
  = NodeBySource
      { nodeSource :: NodeSource
      , indexInProblem :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

data NodeSource
  = NodeSource
      { tag :: Tag
      , functionName :: Ident
      , nodeAddr :: NodeAddr
      }
  deriving (Eq, Generic, Ord, Show)

newtype Problems
  = Problems (M.Map PairingId Problem)
  deriving (Eq, Generic, Ord, Show)

--

instance ParseFile Problems where
    parseFile =
        Problems . M.fromList
            <$> parseBlocksFileWithTypicalKeyFormat ["Problem", "Pairing"] parsePrettyPairingId parseInBlock

instance ParseInBlock Problem where
    parseInBlock = do
        _ <- line $ inLineSymbol "Problem"
        (tagA, sideA) <- problemSideLine
        (tagB, sideB) <- problemSideLine
        (c, asm) <- case (tagA, tagB) of
                (C, Asm) -> return (sideA, sideB)
                (Asm, C) -> return (sideB, sideA)
                _ -> fail "invalid problem side tags"
        nodes <- M.fromList <$> manyTill nodeLine (try endLine)
        return $ Problem { c, asm, nodes }
      where
        nodeLine = line $ (,) <$> parseInLine <*> parseInLine
        endLine = line $ inLineSymbol "EndProblem"
        problemSideLine = line $ do
            _ <- inLineSymbol "Entry"
            entryPoint <- parseInLine
            tag <- parseInLine
            name <- parseInLine
            input <- parseInLine
            output <- parseInLine
            let side = ProblemSide { name, input, output, entryPoint }
            return (tag, side)

instance ParseInLine NodeSource where
    parseInLine = NodeSource <$> parseInLine <*> parseInLine <*> parseInLine
