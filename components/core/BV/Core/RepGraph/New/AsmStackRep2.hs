module BV.Core.RepGraph.New.AsmStackRep2
    ( asmStackRep
    ) where

import BV.Core.RepGraph.New.FlattenGraph

import BV.Core.Types
import BV.Core.Types.Extras

import Data.List (isPrefixOf)

asmStackRep :: Monad m => ArgRenames AsmRefineTag -> VarRepRequestKind -> NameTy -> RepGraphFlattenGraphTaggedT AsmRefineTag m (Maybe VarReqRequest)
asmStackRep argRenames kind var = do
    tag <- askTag
    let cond = and
            [ kind /= VarRepRequestKindInit
            , tag == Asm
            , var.ty == ExprTypeMem
            , "stack" `isPrefixOf` var.name.unwrap -- HACK
            ]
    let spName = argRenames
            (PairingEqSideQuadrant tag PairingEqDirectionIn)
            (Ident "r13")
    return $
        if cond
        then Just $ VarRepRequestSplitMem
            { addr = varE word32T spName
            }
        else Nothing
