{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

module BV.Core.Types.Tag
    ( ByTag (..)
    , ByTag'
    , RefineTag (..)
    , Tag (..)
    , Tag'
    , WithTag (..)
    , WithTag'
    , byTagFromMap
    , numTagValues
    , tagValues
    , viewAtTag
    , viewWithTag
    , withTag
    ) where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Monoid (Ap (..))
import Data.Proxy (Proxy)
import Data.Traversable (foldMapDefault)
import GHC.Generics (Generic)
import Optics (Lens', view)

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

tagValues :: Tag t => [t]
tagValues = [minBound .. maxBound]

numTagValues :: forall t. Tag t => Proxy t -> Int
numTagValues _ = fromEnum (maxBound :: t) - fromEnum (minBound :: t) + 1

byTagFromMap :: Tag t => M.Map t a -> ByTag t a
byTagFromMap m = withTags (pure ()) <&> \(WithTag tag _) -> m M.! tag

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
  = Asm
  | C
  deriving (Bounded, Enum, Eq, Generic, NFData, Ord, Show)

instance Binary RefineTag where

instance Tag RefineTag where
    data ByTag RefineTag a
      = ByRefineTag
          { asm :: a
          , c :: a
          }
      deriving (Eq, Functor, Generic, NFData, Ord, Show)

    atTag = \case
        Asm -> #asm
        C -> #c

    withTags byTag = ByRefineTag
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

instance Applicative (ByTag RefineTag) where
    pure x = ByRefineTag x x
    ff <*> fx = ByRefineTag
        { asm = ff.asm fx.asm
        , c = ff.c fx.c
        }

instance Foldable (ByTag RefineTag) where
    foldMap = foldMapDefault

instance Traversable (ByTag RefineTag) where
    traverse f p = ByRefineTag <$> f p.asm <*> f p.c

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
