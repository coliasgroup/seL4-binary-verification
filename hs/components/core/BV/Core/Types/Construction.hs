module BV.Core.Types.Construction
    ( module BV.Core.Types.Construction.Expr
    , module BV.Core.Types.Construction.Pairing
    , module BV.Core.Types.Construction.ProofCheck
    , programFromFunctions
    , trivialNode
    ) where

import qualified Data.Map as M
import Optics

import BV.Core.Types
import BV.Core.Types.Construction.Expr
import BV.Core.Types.Construction.Pairing
import BV.Core.Types.Construction.ProofCheck

programFromFunctions :: M.Map Ident Function -> Program
programFromFunctions functions = mempty & #functions .~ functions

trivialNode :: NodeId -> Node
trivialNode next = NodeBasic (BasicNode { next, varUpdates = [] })
