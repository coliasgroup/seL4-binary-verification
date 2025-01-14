{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Glue
    ( Input
    ) where

import GHC.Generics (Generic)

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
