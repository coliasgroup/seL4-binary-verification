{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

module BV.Core.Types.Tag
    ( AsmRefineTag (..)
    , ByTag (..)
    , ByTag'
    , RefineTag
    , Tag (..)
    , Tag'
    , WithTag (..)
    , WithTag'
    , byRefineTag
    , byTagFrom
    , byTagFromList
    , getLeft
    , getRight
    , leftTag
    , numTagValues
    , rightTag
    , tagValues
    , viewAtTag
    , viewWithTag
    , withTag
    ) where

import BV.Core.Utils (ensure, zipWithTraversable)

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Monoid (Ap (..))
import Data.Proxy (Proxy (Proxy))
import Data.Traversable (foldMapDefault)
import GHC.Generics (Generic)
import GHC.IsList (IsList (..))
import Optics

class
    ( Eq t
    , Ord t
    , Show t
    , Enum t
    , Bounded t
    , Applicative (ByTag t)
    , Traversable (ByTag t)
    , forall a. NFData a => NFData (ByTag t a)
    , forall a. Ord a => Ord (ByTag t a)
    , forall a. Eq a => Eq (ByTag t a)
    , forall a. Show a => Show (ByTag t a)
    ) => Tag t where

    data ByTag t a

    atTag :: t -> Lens' (ByTag t a) a

    withTags :: ByTag t a -> ByTag t (WithTag t a)

    prettyTag :: t -> String

    parsePrettyTag :: String -> Maybe t

    default prettyTag :: Show t => t -> String
    prettyTag = show

    default parsePrettyTag :: Read t => String -> Maybe t
    parsePrettyTag = Just . read

instance (Tag t, Semigroup m) => Semigroup (ByTag t m) where
    x <> y = getAp $ Ap x <> Ap y

instance (Tag t, Monoid m) => Monoid (ByTag t m) where
    mempty = getAp mempty

instance Tag t => IsList (ByTag t a) where
    type Item (ByTag t a) = a

    toList = toListOf folded -- avoid name conflict with Data.Foldable.toList

    fromListN n xs =
        ensure
            (n == numTagValues (Proxy :: Proxy t))
            (zipWithTraversable const xs (pure ()))

    fromList xs = fromListN (length xs) xs

tagValues :: Tag t => [t]
tagValues = [minBound .. maxBound]

numTagValues :: forall t. Tag t => Proxy t -> Int
numTagValues _ = fromEnum (maxBound :: t) - fromEnum (minBound :: t) + 1

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

byTagFrom :: Tag t => (t -> a) -> ByTag t a
byTagFrom f = withTags (pure ()) <&> \(WithTag t _) -> f t

byTagFromList :: Tag t => [a] -> ByTag t a
byTagFromList = fromList

--

class Tag t => RefineTag t where

byRefineTag :: RefineTag t => a -> a -> ByTag t a
byRefineTag left right = byTagFrom $ \t -> if t == leftTag then left else right

leftTag :: RefineTag t => t
leftTag = minBound

rightTag :: RefineTag t => t
rightTag = maxBound

getLeft :: RefineTag t => ByTag t a -> a
getLeft = view (atTag leftTag)

getRight :: RefineTag t => ByTag t a -> a
getRight = view (atTag rightTag)

--

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

--

data AsmRefineTag
  = Asm
  | C
  deriving (Bounded, Enum, Eq, Generic, NFData, Ord, Show)

instance Binary AsmRefineTag where

instance Tag AsmRefineTag where
    data ByTag AsmRefineTag a
      = ByAsmRefineTag
          { asm :: a
          , c :: a
          }
      deriving (Eq, Functor, Generic, NFData, Ord, Show)

    atTag = \case
        Asm -> #asm
        C -> #c

    withTags byTag = ByAsmRefineTag
        { asm = WithTag Asm byTag.asm
        , c = WithTag C byTag.c
        }

    prettyTag = \case
        Asm -> "ASM"
        C -> "C"

    parsePrettyTag = \case
        "ASM" -> Just Asm
        "C" -> Just C
        _ -> Nothing

instance RefineTag AsmRefineTag

instance Applicative (ByTag AsmRefineTag) where
    pure x = ByAsmRefineTag x x
    ff <*> fx = ByAsmRefineTag
        { asm = ff.asm fx.asm
        , c = ff.c fx.c
        }

instance Foldable (ByTag AsmRefineTag) where
    foldMap = foldMapDefault

instance Traversable (ByTag AsmRefineTag) where
    traverse f p = ByAsmRefineTag <$> f p.asm <*> f p.c

instance Binary a => Binary (ByTag AsmRefineTag a) where

-- TODO

type ByTag' = ByTag AsmRefineTag

type WithTag' = WithTag AsmRefineTag

type Tag' = AsmRefineTag
