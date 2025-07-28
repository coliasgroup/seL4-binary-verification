{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.Problem
    ( NodeBySource (..)
    , NodeSource (..)
    , Problem (..)
    , ProblemSide (..)
    , Problems (..)
    ) where

import BV.Core.Types.Pairing
import BV.Core.Types.Program
import BV.Core.Types.Tag

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import qualified Data.Map as M
import GHC.Generics (Generic)

data Problem
  = Problem
      { sides :: ByTag' ProblemSide
      , nodes :: NodeMap
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data ProblemSide
  = ProblemSide
      { name :: Ident
      , input :: [Argument]
      , output :: [Argument]
      , entryPoint :: NodeId
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data NodeBySource
  = NodeBySource
      { nodeSource :: NodeSource
      , indexInProblem :: Int
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary NodeBySource where

data NodeSource
  = NodeSource
      { tag :: Tag'
      , functionName :: Ident
      , nodeAddr :: NodeAddr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary NodeSource where

newtype Problems
  = Problems { unwrap :: M.Map PairingId Problem }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance AtPairingId Problem Problems where
    atPairingId = atPairingId . (.unwrap)
