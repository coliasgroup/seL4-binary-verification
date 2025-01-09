{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Check
    ( Input
    ) where

import Control.Monad.Logger (MonadLogger)
import GHC.Generics (Generic)
import Optics.Core

import SimpleSMT.Abstract (MonadSolver)

import BV.Core.Environment
import BV.Core.Types

data Input
  = Input
      { programs :: PairingOf Program
      , objDumpInfo :: ObjDumpInfo
      , stackBounds :: StackBounds
      , inlineScripts :: InlineScripts
      }
  deriving (Eq, Generic, Ord, Show)

enumerateAllProofChecks :: MonadCheckWriteOnly m => Input -> m (ProofChecks String)
enumerateAllProofChecks = undefined
  where
    x = undefined
