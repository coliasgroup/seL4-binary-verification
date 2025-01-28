{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module BV.Core.Types.ProofScript
    ( CaseSplitProofNode (..)
    , Lambda (..)
    , ProofNode (..)
    , ProofNodeWith (..)
    , ProofScript (..)
    , RestrProofNode (..)
    , RestrProofNodeRange (..)
    , RestrProofNodeRangeKind (..)
    , SingleRevInductProofNode (..)
    , SplitProofNode (..)
    , SplitProofNodeDetails (..)
    , prettyRestrProofNodeRangeKind
    , traverseCaseSplitProofNodeChildren
    , traverseRestrProofNodeChild
    , traverseSingleRevInductProofNodeChild
    , traverseSplitProofNodeChildren
    ) where

import BV.Core.Types.Pairing
import BV.Core.Types.Program

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

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

prettyRestrProofNodeRangeKind :: RestrProofNodeRangeKind -> String
prettyRestrProofNodeRangeKind = \case
    RestrProofNodeRangeKindNumber -> "Number"
    RestrProofNodeRangeKindOffset -> "Offset"

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
      { n :: Integer
      , loopRMax :: Integer
      , details :: PairingOf SplitProofNodeDetails
      , eqs :: [(Lambda, Lambda)]
      , p1 :: ProofNodeWith a
      , p2 :: ProofNodeWith a
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data SplitProofNodeDetails
  = SplitProofNodeDetails
      { split :: NodeAddr
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
      , eqs :: [Lambda]
      , pred_ :: Expr
      , nBound :: Integer
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

traverseRestrProofNodeChild
    :: Applicative f
    => (ProofNodeWith a -> f (ProofNodeWith b))
    -> RestrProofNode a
    -> f (RestrProofNode b)
traverseRestrProofNodeChild f (RestrProofNode {..}) =
    g <$> f child
  where
    g child' = RestrProofNode
        { child = child'
        , ..
        }

traverseCaseSplitProofNodeChildren
    :: Applicative f
    => (ProofNodeWith a -> f (ProofNodeWith b))
    -> (ProofNodeWith a -> f (ProofNodeWith b))
    -> CaseSplitProofNode a
    -> f (CaseSplitProofNode b)
traverseCaseSplitProofNodeChildren f1 f2 (CaseSplitProofNode {..}) =
    g <$> f1 left <*> f2 right
  where
    g left' right' = CaseSplitProofNode
        { left = left'
        , right = right'
        , ..
        }

traverseSplitProofNodeChildren
    :: Applicative f
    => (ProofNodeWith a -> f (ProofNodeWith b))
    -> (ProofNodeWith a -> f (ProofNodeWith b))
    -> SplitProofNode a
    -> f (SplitProofNode b)
traverseSplitProofNodeChildren f1 f2 (SplitProofNode {..}) =
    g <$> f1 p1 <*> f2 p2
  where
    g p1' p2' = SplitProofNode
        { p1 = p1'
        , p2 = p2'
        , ..
        }

traverseSingleRevInductProofNodeChild
    :: Applicative f
    => (ProofNodeWith a -> f (ProofNodeWith b))
    -> SingleRevInductProofNode a
    -> f (SingleRevInductProofNode b)
traverseSingleRevInductProofNodeChild f (SingleRevInductProofNode {..}) =
    g <$> f child
  where
    g child' = SingleRevInductProofNode
        { child = child'
        , ..
        }
