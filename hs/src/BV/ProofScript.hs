module BV.ProofScript where

import GHC.Generics (Generic)

import BV.Pairing
import BV.Problem
import BV.Program

data ProofNode
  = Leaf
  | Restr RestrProofNode
  | CaseSplit CaseSplitProofNode
  | Split SplitProofNode
  | SingleRevInduct SingleRevInductProofNode
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
      , freeVarTy :: Ident
      , expr :: Expr
      }
  deriving (Eq, Generic, Ord, Show)
