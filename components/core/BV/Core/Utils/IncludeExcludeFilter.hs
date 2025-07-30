{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Utils.IncludeExcludeFilter
    ( IncludeExcludeFilter (..)
    , applyIncludeExcludeFilter
    ) where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)

data IncludeExcludeFilter a
  = IncludeExcludeFilter
      { include :: Maybe (Set a)
      , exclude :: Set a
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary a => Binary (IncludeExcludeFilter a)

applyIncludeExcludeFilter :: Ord a => IncludeExcludeFilter a -> a -> Bool
applyIncludeExcludeFilter f a = maybe (const True) (flip S.member) f.include a && a `S.notMember` f.exclude
