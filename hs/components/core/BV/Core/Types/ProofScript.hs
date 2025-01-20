{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.ProofScript where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import BV.Core.Types.Pairing
import BV.Core.Types.Program

newtype ProofScript a
  = ProofScript { root :: ProofNodeWith a }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving newtype (NFData)

data ProofNodeWith a
  = ProofNodeWith
      { meta :: a
      , node :: ProofNode a
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data ProofNode a
  = ProofNodeLeaf
  | ProofNodeRestr (RestrProofNode a)
  | ProofNodeCaseSplit (CaseSplitProofNode a)
  | ProofNodeSplit (SplitProofNode a)
  | ProofNodeSingleRevInduct (SingleRevInductProofNode a)
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data RestrProofNode a
  = RestrProofNode
      { point :: NodeAddr
      , tag :: Tag
      , range :: RestrProofNodeRange
      , child :: ProofNodeWith a
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data RestrProofNodeRange
  = RestrProofNodeRange
      { kind :: RestrProofNodeRangeKind
      , x :: Integer
      , y :: Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data RestrProofNodeRangeKind
  = RestrProofNodeRangeKindNumber
  | RestrProofNodeRangeKindOffset
  deriving (Eq, Generic, NFData, Ord, Show)

data CaseSplitProofNode a
  = CaseSplitProofNode
      { addr :: NodeAddr
      , tag :: Tag
      , left :: ProofNodeWith a
      , right :: ProofNodeWith a
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data SplitProofNode a
  = SplitProofNode
      { addr :: NodeAddr
      , loopRMax :: Integer
      , rDetails :: SplitProofNodeDetails
      , lDetails :: SplitProofNodeDetails
      , eqs :: [(Lambda, Lambda)]
      , p1 :: ProofNodeWith a
      , p2 :: ProofNodeWith a
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data SplitProofNodeDetails
  = SplitProofNodeDetails
      { split :: Integer
      , seqStart :: Integer
      , step :: Integer
      , eqs :: [Lambda]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data SingleRevInductProofNode a
  = SingleRevInductProofNode
      { point :: NodeAddr
      , tag :: Tag
      , n :: Integer
      , egs :: [Lambda]
      , pred :: Expr
      , nBounds :: Integer
      , child :: ProofNodeWith a
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data Lambda
  = Lambda
      { freeVar :: Ident
      , freeVarTy :: ExprType
      , expr :: Expr
      }
  deriving (Eq, Generic, NFData, Ord, Show)
