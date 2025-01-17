{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BV.Core.Logic
    ( alignOfType
    , isNodeNoop
    , sizeOfType
    , trivialNode
    ) where

import BV.Core.Arch
import BV.Core.Types
import Control.Exception (assert)

sizeOfType :: ExprType -> Integer
sizeOfType = \case
    ExprTypeArray { ty, length } -> length * sizeOfType ty
    ExprTypeWord bits -> assert (bits `mod` 8 == 0) $ bits `div`  8
    ExprTypePtr _ -> archPtrSizeBytes
    _ -> error ""

alignOfType :: ExprType -> Integer
alignOfType ty = case ty of
    ExprTypeArray { ty } -> alignOfType ty
    ExprTypeWord _ -> sizeOfType ty
    ExprTypePtr _ -> sizeOfType ty
    _ -> error ""

isNodeNoop :: Node -> Bool
isNodeNoop = \case
    NodeBasic (BasicNode { varUpdates }) -> null varUpdates
    NodeCond (CondNode { left, right }) -> left == right
    NodeCall (CallNode {}) -> False


trivialNode :: NodeId -> Node
trivialNode next = NodeBasic (BasicNode { next, varUpdates = [] })
