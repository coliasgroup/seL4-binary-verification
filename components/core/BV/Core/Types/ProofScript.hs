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

import BV.Core.Types.Program
import BV.Core.Types.Tag

import Control.DeepSeq (NFData, NFData1)
import Data.Binary (Binary)
import GHC.Generics (Generic, Generic1)

newtype ProofScript a
  = ProofScript { root :: ProofNodeWith a }
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)
  deriving newtype (NFData, NFData1)

instance Binary a => Binary (ProofScript a) where

data ProofNodeWith a
  = ProofNodeWith
      { meta :: a
      , node :: ProofNode a
      }
  deriving
    ( Eq
    , Foldable
    , Functor
    , Generic
    , Generic1
    , NFData
    , NFData1
    , Ord
    , Show
    , Traversable
    )

instance Binary a => Binary (ProofNodeWith a) where

data ProofNode a
  = ProofNodeLeaf
  | ProofNodeRestr (RestrProofNode a)
  | ProofNodeCaseSplit (CaseSplitProofNode a)
  | ProofNodeSplit (SplitProofNode a)
  | ProofNodeSingleRevInduct (SingleRevInductProofNode a)
  deriving
    ( Eq
    , Foldable
    , Functor
    , Generic
    , Generic1
    , NFData
    , NFData1
    , Ord
    , Show
    , Traversable
    )

instance Binary a => Binary (ProofNode a) where

data RestrProofNode a
  = RestrProofNode
      { point :: NodeAddr
      , tag :: Tag'
      , range :: RestrProofNodeRange
      , child :: ProofNodeWith a
      }
  deriving
    ( Eq
    , Foldable
    , Functor
    , Generic
    , Generic1
    , NFData
    , NFData1
    , Ord
    , Show
    , Traversable
    )

instance Binary a => Binary (RestrProofNode a) where

data RestrProofNodeRange
  = RestrProofNodeRange
      { kind :: RestrProofNodeRangeKind
      , x :: Integer
      , y :: Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary RestrProofNodeRange where

data RestrProofNodeRangeKind
  = RestrProofNodeRangeKindNumber
  | RestrProofNodeRangeKindOffset
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary RestrProofNodeRangeKind where

prettyRestrProofNodeRangeKind :: RestrProofNodeRangeKind -> String
prettyRestrProofNodeRangeKind = \case
    RestrProofNodeRangeKindNumber -> "Number"
    RestrProofNodeRangeKindOffset -> "Offset"

data CaseSplitProofNode a
  = CaseSplitProofNode
      { addr :: NodeAddr
      , tag :: Tag'
      , left :: ProofNodeWith a
      , right :: ProofNodeWith a
      }
  deriving
    ( Eq
    , Foldable
    , Functor
    , Generic
    , Generic1
    , NFData
    , NFData1
    , Ord
    , Show
    , Traversable
    )

instance Binary a => Binary (CaseSplitProofNode a) where

data SplitProofNode a
  = SplitProofNode
      { n :: Integer
      , loopRMax :: Integer
      , details :: ByTag' SplitProofNodeDetails
      , eqs :: [(Lambda, Lambda)]
      , p1 :: ProofNodeWith a
      , p2 :: ProofNodeWith a
      }
  deriving
    ( Eq
    , Foldable
    , Functor
    , Generic
    , Generic1
    , NFData
    , NFData1
    , Ord
    , Show
    , Traversable
    )

instance Binary a => Binary (SplitProofNode a) where

data SplitProofNodeDetails
  = SplitProofNodeDetails
      { split :: NodeAddr
      , seqStart :: Integer
      , step :: Integer
      , eqs :: [Lambda]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary SplitProofNodeDetails where

data SingleRevInductProofNode a
  = SingleRevInductProofNode
      { point :: NodeAddr
      , tag :: Tag'
      , n :: Integer
      , eqs :: [Lambda]
      , pred_ :: Expr
      , nBound :: Integer
      , child :: ProofNodeWith a
      }
  deriving
    ( Eq
    , Foldable
    , Functor
    , Generic
    , Generic1
    , NFData
    , NFData1
    , Ord
    , Show
    , Traversable
    )

instance Binary a => Binary (SingleRevInductProofNode a) where

data Lambda
  = Lambda
      { freeVar :: Ident
      , freeVarTy :: ExprType
      , expr :: Expr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary Lambda where

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
