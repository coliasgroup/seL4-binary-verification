{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Search.Core.StackBounds
    ( DiscoverStackBoundsInput (..)
    , discoverStackBounds
    ) where

import BV.Core.Types
import BV.Logging
import BV.Search.Core.Solver

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
    :: (Monad m, MonadRepGraphSolverInteract n, MonadLoggerWithContext m, MonadLoggerWithContext n)
    => (forall a. n a -> m a)
    -> DiscoverStackBoundsInput
    -> m StackBounds
discoverStackBounds run input = undefined
