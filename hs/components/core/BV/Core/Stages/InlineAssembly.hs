module BV.Core.Stages.InlineAssembly
    ( addInlineAssemblySpecs
    ) where

import qualified Data.Map as M

import BV.Core.Types

addInlineAssemblySpecs :: PairingOf Program -> (Pairings, PairingOf Program)
addInlineAssemblySpecs progs = (pairings, progs)
  where
    pairings = Pairings M.empty
