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
import BV.Utils (formatArgSimple)

import Control.DeepSeq (NFData, NFData1)
import Data.Binary (Binary)
import GHC.Generics (Generic, Generic1)
import Text.Printf (PrintfArg (formatArg))

newtype ProofScript t a
  = ProofScript { root :: ProofNodeWith t a }
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)
  deriving newtype (NFData, NFData1)

instance (Tag t, Binary t, Binary a) => Binary (ProofScript t a)

data ProofNodeWith t a
  = ProofNodeWith
      { meta :: a
      , node :: ProofNode t a
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

instance (Tag t, Binary t, Binary a) => Binary (ProofNodeWith t a)

data ProofNode t a
  = ProofNodeLeaf
  | ProofNodeRestr (RestrProofNode t a)
  | ProofNodeCaseSplit (CaseSplitProofNode t a)
  | ProofNodeSplit (SplitProofNode t a)
  | ProofNodeSingleRevInduct (SingleRevInductProofNode t a)
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

instance (Tag t, Binary t, Binary a) => Binary (ProofNode t a)

data RestrProofNode t a
  = RestrProofNode
      { point :: NodeAddr
      , tag :: t
      , range :: RestrProofNodeRange
      , child :: ProofNodeWith t a
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

instance (Tag t, Binary t, Binary a) => Binary (RestrProofNode t a)

data RestrProofNodeRange
  = RestrProofNodeRange
      { kind :: RestrProofNodeRangeKind
      , x :: Integer
      , y :: Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary RestrProofNodeRange

data RestrProofNodeRangeKind
  = RestrProofNodeRangeKindNumber
  | RestrProofNodeRangeKindOffset
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary RestrProofNodeRangeKind

prettyRestrProofNodeRangeKind :: RestrProofNodeRangeKind -> String
prettyRestrProofNodeRangeKind = \case
    RestrProofNodeRangeKindNumber -> "Number"
    RestrProofNodeRangeKindOffset -> "Offset"

instance PrintfArg RestrProofNodeRangeKind where
    formatArg = formatArgSimple prettyRestrProofNodeRangeKind

data CaseSplitProofNode t a
  = CaseSplitProofNode
      { addr :: NodeAddr
      , tag :: t
      , left :: ProofNodeWith t a
      , right :: ProofNodeWith t a
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

instance (Tag t, Binary t, Binary a) => Binary (CaseSplitProofNode t a)

data SplitProofNode t a
  = SplitProofNode
      { n :: Integer
      , loopRMax :: Integer
      , details :: ByTag t SplitProofNodeDetails
      , eqs :: [(Lambda, Lambda)]
      , p1 :: ProofNodeWith t a
      , p2 :: ProofNodeWith t a
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

instance (Tag t, Binary t, Binary a) => Binary (SplitProofNode t a)

data SplitProofNodeDetails
  = SplitProofNodeDetails
      { split :: NodeAddr
      , seqStart :: Integer
      , step :: Integer
      , eqs :: [Lambda]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary SplitProofNodeDetails

data SingleRevInductProofNode t a
  = SingleRevInductProofNode
      { point :: NodeAddr
      , tag :: t
      , n :: Integer
      , eqs :: [Lambda]
      , pred_ :: Expr
      , nBound :: Integer
      , child :: ProofNodeWith t a
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

instance (Tag t, Binary t, Binary a) => Binary (SingleRevInductProofNode t a)

data Lambda
  = Lambda
      { freeVar :: Ident
      , freeVarTy :: ExprType
      , expr :: Expr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary Lambda

traverseRestrProofNodeChild
    :: Applicative f
    => (ProofNodeWith t a -> f (ProofNodeWith t b))
    -> RestrProofNode t a
    -> f (RestrProofNode t b)
traverseRestrProofNodeChild f (RestrProofNode {..}) =
    g <$> f child
  where
    g child' = RestrProofNode
        { child = child'
        , ..
        }

traverseCaseSplitProofNodeChildren
    :: Applicative f
    => (ProofNodeWith t a -> f (ProofNodeWith t b))
    -> (ProofNodeWith t a -> f (ProofNodeWith t b))
    -> CaseSplitProofNode t a
    -> f (CaseSplitProofNode t b)
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
    => (ProofNodeWith t a -> f (ProofNodeWith t b))
    -> (ProofNodeWith t a -> f (ProofNodeWith t b))
    -> SplitProofNode t a
    -> f (SplitProofNode t b)
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
    => (ProofNodeWith t a -> f (ProofNodeWith t b))
    -> SingleRevInductProofNode t a
    -> f (SingleRevInductProofNode t b)
traverseSingleRevInductProofNodeChild f (SingleRevInductProofNode {..}) =
    g <$> f child
  where
    g child' = SingleRevInductProofNode
        { child = child'
        , ..
        }
