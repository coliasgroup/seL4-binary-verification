{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module BV.Core.Types.Tag where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Monoid (Ap (..))
import Data.Traversable (foldMapDefault)
import GHC.Generics (Generic)
import Optics (Lens', view)

class (Enum t, Bounded t, Applicative (ByTag t), Traversable (ByTag t)) => Tag t where
    data ByTag t a

    atTag :: t -> Lens' (ByTag t a) a

    withTags :: ByTag t a -> ByTag t (WithTag t a)

    prettyTag :: t -> String

    default prettyTag :: Show t => t -> String
    prettyTag = show

instance (Tag t, Semigroup m) => Semigroup (ByTag t m) where
    x <> y = getAp $ Ap x <> Ap y

instance (Tag t, Monoid m) => Monoid (ByTag t m) where
    mempty = getAp mempty

tagValues :: Tag t => [t]
tagValues = [minBound .. maxBound]

instance Tag () where
    newtype ByTag () a = ByUnitTag { unit :: a }
      deriving (Eq, Generic, Ord, Show, Functor, Foldable, Traversable)
      deriving newtype (NFData, Binary)

    atTag () = #unit

    withTags byTag = ByUnitTag
        { unit = WithTag () byTag.unit
        }

instance Applicative (ByTag ()) where
    pure = ByUnitTag
    (ByUnitTag f) <*> (ByUnitTag a) = ByUnitTag (f a)

data RefineTag
  = C
  | Asm
  deriving (Bounded, Enum, Eq, Generic, NFData, Ord, Show)

instance Binary RefineTag where

instance Tag RefineTag where
    data ByTag RefineTag a
      = ByRefineTag
          { c :: a
          , asm :: a
          }
      deriving (Eq, Functor, Generic, NFData, Ord, Show)

    atTag = \case
        C -> #c
        Asm -> #asm

    withTags byTag = ByRefineTag
        { asm = WithTag Asm byTag.asm
        , c = WithTag C byTag.c
        }

    prettyTag = \case
        C -> "C"
        Asm -> "ASM"

instance Applicative (ByTag RefineTag) where
    pure x = ByRefineTag x x
    ff <*> fx = ByRefineTag
        { c = ff.c fx.c
        , asm = ff.asm fx.asm
        }

instance Foldable (ByTag RefineTag) where
    foldMap = foldMapDefault

instance Traversable (ByTag RefineTag) where
    traverse f p = (\c asm -> ByRefineTag { c, asm }) <$> f p.c <*> f p.asm

instance Binary a => Binary (ByTag RefineTag a) where

data WithTag t a
  = WithTag
      { tag :: t
      , value :: a
      }
  deriving (Eq, Functor, Generic, NFData, Ord, Show)

withTag :: (t -> a -> b) -> WithTag t a -> b
withTag f (WithTag tag value) = f tag value

viewAtTag :: Tag t => t -> ByTag t a -> a
viewAtTag = view . atTag

viewWithTag :: Tag t => t -> ByTag t a -> WithTag t a
viewWithTag tag = viewAtTag tag . withTags

-- TODO

type ByTag' = ByTag RefineTag

type WithTag' = WithTag RefineTag

type Tag' = RefineTag
