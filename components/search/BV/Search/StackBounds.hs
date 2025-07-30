{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Search.StackBounds
    ( DiscoverStackBoundsInput (..)
    , discoverStackBounds
    ) where

import BV.Core.ModelConfig (ModelConfig)
import BV.Core.Types
import BV.SMTLIB2.Monad

import qualified Data.Set as S
import GHC.Generics (Generic)

data DiscoverStackBoundsInput
  = DiscoverStackBoundsInput
      { rodata :: ROData
      , functions :: Ident -> Function
      , include :: S.Set Ident
      }
  deriving (Generic)

discoverStackBounds
    :: (Monad m, MonadSolver n)
    => ((ModelConfig -> n a) -> m a)
    -> DiscoverStackBoundsInput
    -> m StackBounds
discoverStackBounds run input = undefined
