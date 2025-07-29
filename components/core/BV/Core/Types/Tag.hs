{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.Tag
    ( AsmRefineTag (..)
    , ByAsmRefineTag (..)
    , ByTag
    , ByTag'
    , RefineTag
    , Tag (..)
    , TrivialTag (..)
    , WithTag (..)
    , WithTag'
    , atTag
    , byAsmRefineTag
    , byRefineTag
    , byTagFrom
    , byTagFromList
    , getAsm
    , getC
    , getLeft
    , getRight
    , leftTag
    , numTagValues
    , rightTag
    , tagValues
    , viewAtTag
    , viewWithTag
    , withTag
    , withTags
    ) where

import BV.Core.Utils (expectingIx)

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Foldable (toList)
import Data.Monoid (Ap (..))
import Data.Proxy (Proxy (Proxy))
import qualified Data.Vector as V
import Data.Vector.Binary ()
import GHC.Generics (Generic)
import GHC.IsList (IsList, fromList)
import Optics

class
    ( Eq t
    , Ord t
    , Show t
    , Enum t
    , Bounded t
    , NFData t
    , Generic t
    ) => Tag t where

    prettyTag :: t -> String

    parsePrettyTag :: String -> Maybe t

    prettyTag = show

    default parsePrettyTag :: Read t => String -> Maybe t
    parsePrettyTag = Just . read

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

--

newtype ByTag t a
  = ByTag { unwrap :: V.Vector a }
  deriving (Eq, Foldable, Functor, Generic, Ord, Traversable)
  deriving newtype (IsList, NFData)

instance (Tag t, Show a) => Show (ByTag t a) where
    show byTag = show $ toList $ withTag (,) <$> withTags byTag

instance Binary a => Binary (ByTag t a) where

instance Tag t => Applicative (ByTag t) where
    pure = ByTag . V.replicate (numTagValues (Proxy :: Proxy t))
    (ByTag f) <*> (ByTag a) = ByTag $ V.zipWith id f a

instance (Tag t, Semigroup m) => Semigroup (ByTag t m) where
    x <> y = getAp $ Ap x <> Ap y

instance (Tag t, Monoid m) => Monoid (ByTag t m) where
    mempty = getAp mempty

atTag :: Tag t => t -> Lens' (ByTag t a) a
atTag t = #unwrap % expectingIx (fromEnum t)

withTags :: Tag t => ByTag t a -> ByTag t (WithTag t a)
withTags = #unwrap %~ imap (WithTag . toEnum)

viewAtTag :: Tag t => t -> ByTag t a -> a
viewAtTag = view . atTag

viewWithTag :: Tag t => t -> ByTag t a -> WithTag t a
viewWithTag tag = viewAtTag tag . withTags

byTagFrom :: Tag t => (t -> a) -> ByTag t a
byTagFrom f = withTag (&) <$> withTags (pure f)

byTagFromList :: Tag t => [a] -> ByTag t a
byTagFromList = fromList

--

class Tag t => RefineTag t where

byRefineTag :: RefineTag t => a -> a -> ByTag t a
byRefineTag left right = byTagFromList [left, right]

leftTag :: RefineTag t => t
leftTag = minBound

rightTag :: RefineTag t => t
rightTag = maxBound

getLeft :: RefineTag t => ByTag t a -> a
getLeft = view (atTag leftTag)

getRight :: RefineTag t => ByTag t a -> a
getRight = view (atTag rightTag)

--

data TrivialTag
  = Target
  deriving (Bounded, Enum, Eq, Generic, NFData, Ord, Read, Show)

instance Tag TrivialTag where

--

data AsmRefineTag
  = Asm
  | C
  deriving (Bounded, Enum, Eq, Generic, NFData, Ord, Show)

instance Binary AsmRefineTag where

instance Tag AsmRefineTag where
    prettyTag = \case
        Asm -> "ASM"
        C -> "C"
    parsePrettyTag = \case
        "ASM" -> Just Asm
        "C" -> Just C
        _ -> Nothing

instance RefineTag AsmRefineTag

getAsm :: ByTag AsmRefineTag a -> a
getAsm = getLeft

getC :: ByTag AsmRefineTag a -> a
getC = getRight

-- TODO
-- {-# LANGUAGE PatternSynonyms #-}
-- pattern ByAsmRefineTag :: a -> a -> ByTag AsmRefineTag a
-- pattern ByAsmRefineTag { asm, c } <- TODO

data ByAsmRefineTag a
  = ByAsmRefineTag
      { asm :: a
      , c :: a
      }
  deriving (Eq, Generic, NFData, Ord, Show)

byAsmRefineTag :: ByAsmRefineTag a -> ByTag AsmRefineTag a
byAsmRefineTag (ByAsmRefineTag { asm, c }) = fromList [asm, c]

-- TODO

type ByTag' = ByTag AsmRefineTag

type WithTag' = WithTag AsmRefineTag
