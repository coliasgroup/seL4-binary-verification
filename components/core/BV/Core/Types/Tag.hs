{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module BV.Core.Types.Tag
    ( ByTag
    , RefineTag
    , StaticTag
    , Tag (..)
    , TrivialTag (..)
    , WithTag (..)
    , atTag
    , byRefineTag
    , byTagFrom
    , byTagFromList
    , byTagFromListUnchecked
    , byTagFromN
    , getLeft
    , getRight
    , leftTag
    , numTagValues
    , otherTag
    , rightTag
    , tagValues
    , viewAtTag
    , viewByRefineTag
    , viewWithTag
    , withByRefineTag
    , withTag
    , withTags
    ) where

import BV.Utils (ensure, expectingIx, formatArgSimple)

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Foldable (toList)
import Data.Monoid (Ap (..))
import Data.Proxy (Proxy (Proxy))
import qualified Data.Vector as V
import Data.Vector.Binary ()
import GHC.Generics (Generic)
import qualified GHC.IsList as IsList
import GHC.Records (HasField (getField))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Optics
import Text.Printf (PrintfArg (formatArg))

class
    ( Eq t
    , Ord t
    , Show t
    , Enum t
    , NFData t
    , Generic t
    , PrintfArg t
    ) => Tag t where

    prettyTag :: t -> String

    parsePrettyTag :: String -> Maybe t

    prettyTag = show

    default parsePrettyTag :: Read t => String -> Maybe t
    parsePrettyTag = Just . read

class (Tag t, Bounded t) => StaticTag t where

tagValues :: StaticTag t => [t]
tagValues = [minBound .. maxBound]

numTagValues :: forall t. StaticTag t => Proxy t -> Int
numTagValues _ = fromEnum (maxBound :: t) - fromEnum (minBound :: t) + 1

data WithTag t a
  = WithTag
      { tag :: t
      , value :: a
      }
  deriving (Eq, Functor, Generic, NFData, Ord, Show)

withTag :: (t -> a -> b) -> WithTag t a -> b
withTag f (WithTag tag value) = f tag value

--

newtype ByTag t a
  = ByTag { unwrap :: V.Vector a }
  deriving (Eq, Foldable, Functor, Generic, Ord, Traversable)
  deriving newtype (NFData)

instance (Tag t, Show a) => Show (ByTag t a) where
    show byTag = show $ toList $ withTag (,) <$> withTags byTag

instance Binary a => Binary (ByTag t a)

instance StaticTag t => IsList.IsList (ByTag t a) where
    type Item (ByTag t a) = a
    fromList = byTagFromList
    toList (ByTag v) = V.toList v

instance StaticTag t => Applicative (ByTag t) where
    pure = ByTag . V.replicate (numTagValues (Proxy :: Proxy t))
    (ByTag f) <*> (ByTag a) = ByTag $ V.zipWith id f a

instance (StaticTag t, Semigroup m) => Semigroup (ByTag t m) where
    x <> y = getAp $ Ap x <> Ap y

instance (StaticTag t, Monoid m) => Monoid (ByTag t m) where
    mempty = getAp mempty

atTag :: Tag t => t -> Lens' (ByTag t a) a
atTag t = #unwrap % expectingIx (fromEnum t)

withTags :: Tag t => ByTag t a -> ByTag t (WithTag t a)
withTags = #unwrap %~ imap (WithTag . toEnum)

viewAtTag :: Tag t => t -> ByTag t a -> a
viewAtTag = view . atTag

viewWithTag :: Tag t => t -> ByTag t a -> WithTag t a
viewWithTag tag = viewAtTag tag . withTags

byTagFrom :: StaticTag t => (t -> a) -> ByTag t a
byTagFrom f = withTag (&) <$> withTags (pure f)

byTagFromList :: forall t a. StaticTag t => [a] -> ByTag t a
byTagFromList xs = ensure (V.length v == numTagValues (Proxy :: Proxy t)) $ ByTag v
  where
    v = V.fromList xs

byTagFromN :: Tag t => Int -> (t -> a) -> ByTag t a
byTagFromN n f = withTag (&) <$> withTags (ByTag (V.replicate n f))

byTagFromListUnchecked :: Tag t => [a] -> ByTag t a
byTagFromListUnchecked = ByTag . V.fromList

--

class StaticTag t => RefineTag t where

byRefineTag :: RefineTag t => a -> a -> ByTag t a
byRefineTag left right = IsList.fromList [left, right]

withByRefineTag :: RefineTag t => (a -> a -> b) -> ByTag t a -> b
withByRefineTag f byTag = f byTag.left byTag.right

viewByRefineTag :: RefineTag t => ByTag t a -> (a, a)
viewByRefineTag = withByRefineTag (,)

leftTag :: RefineTag t => t
leftTag = minBound

rightTag :: RefineTag t => t
rightTag = maxBound

otherTag :: RefineTag t => t -> t
otherTag tag = if tag == leftTag then rightTag else leftTag

getLeft :: RefineTag t => ByTag t a -> a
getLeft = view (atTag leftTag)

getRight :: RefineTag t => ByTag t a -> a
getRight = view (atTag rightTag)

instance RefineTag t => HasField "left" (ByTag t a) a where
  getField = getLeft

instance RefineTag t => HasField "right" (ByTag t a) a where
  getField = getRight

--

data TrivialTag (name :: Symbol)
  = TrivialTag
  deriving (Bounded, Enum, Eq, Generic, NFData, Ord, Read, Show)

instance KnownSymbol name => PrintfArg (TrivialTag name) where
    formatArg = formatArgSimple prettyTag

instance KnownSymbol name => Tag (TrivialTag name) where
    prettyTag _ = symbolVal (Proxy :: Proxy name)
    parsePrettyTag s =
        if s == symbolVal (Proxy :: Proxy name)
        then Just TrivialTag
        else Nothing
