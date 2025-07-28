{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.Problem
    ( NodeBySource (..)
    , NodeSource (..)
    , Problem (..)
    , Problem'
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

type Problem' = Problem RefineTag

data Problem t
  = Problem
      { sides :: ByTag t ProblemSide
      , nodes :: NodeMap
      }
  deriving (Generic)

deriving instance Tag t => Eq (Problem t)
deriving instance Tag t => NFData (Problem t)

data ProblemSide
  = ProblemSide
      { name :: Ident
      , input :: [Argument]
      , output :: [Argument]
      , entryPoint :: NodeId
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data NodeBySource t
  = NodeBySource
      { nodeSource :: NodeSource t
      , indexInProblem :: Int
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary t => Binary (NodeBySource t) where

data NodeSource t
  = NodeSource
      { tag :: t
      , functionName :: Ident
      , nodeAddr :: NodeAddr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary t => Binary (NodeSource t) where

newtype Problems
  = Problems { unwrap :: M.Map PairingId Problem' }
  deriving (Eq, Generic)
  deriving newtype (NFData)

instance AtPairingId Problem' Problems where
    atPairingId = atPairingId . (.unwrap)
