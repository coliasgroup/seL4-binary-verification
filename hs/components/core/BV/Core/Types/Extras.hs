module BV.Core.Types.Extras
    ( module BV.Core.Types.Extras.Expr
    , module BV.Core.Types.Extras.Pairing
    , module BV.Core.Types.Extras.ProofCheck
    , module BV.Core.Types.Extras.SExprWithPlaceholders
    , programFromFunctions
    , trivialNode
    ) where

import qualified Data.Map as M
import Optics

import BV.Core.Types
import BV.Core.Types.Extras.Expr
import BV.Core.Types.Extras.Pairing
import BV.Core.Types.Extras.ProofCheck
import BV.Core.Types.Extras.SExprWithPlaceholders

programFromFunctions :: M.Map Ident Function -> Program
programFromFunctions functions = mempty & #functions .~ functions

trivialNode :: NodeId -> Node
trivialNode next = NodeBasic (BasicNode { next, varUpdates = [] })
