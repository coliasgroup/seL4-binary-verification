module BV.Core.Types.Extras.Problem
    ( ArgRenames
    , argRenamesOf
    , pairingIdOfProblem
    , predsOf
    , varNamesOfProblem
    ) where

import BV.Core.Types
import BV.Core.Types.Extras.Program

import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Optics

type ArgRenames = PairingEqSideQuadrant -> Ident -> Ident

argRenamesOf :: (WithTag' Ident -> Function) -> Problem -> ArgRenames
argRenamesOf lookupFunction problem quadrant mangledName =
    fromJust $ lookup mangledName (zip (map (.name) mangledArgs) (map (.name) origArgs))
  where
    fun = lookupFunction (viewWithTag quadrant.tag (pairingIdOfProblem problem))
    origArgs = case quadrant.direction of
        PairingEqDirectionIn -> fun.input
        PairingEqDirectionOut -> fun.output
    probSide = viewAtTag quadrant.tag problem.sides
    mangledArgs = case quadrant.direction of
        PairingEqDirectionIn -> probSide.input
        PairingEqDirectionOut -> probSide.output

pairingIdOfProblem :: Problem -> PairingId
pairingIdOfProblem problem = view #name <$> problem.sides

varNamesOfProblem :: Traversal' Problem Ident
varNamesOfProblem =
    (#sides % traversed % (#input `adjoin` #output) % traversed % varNamesOf)
        `adjoin` (#nodes % traversed % varNamesOf)

predsOf :: Problem -> M.Map NodeId (S.Set NodeAddr)
predsOf problem =
    M.fromListWith (<>) $ concat $ [defaults] ++
        [ [ (cont, S.singleton nodeAddr)
        | cont <- node ^.. nodeConts
        ]
        | (nodeAddr, node) <- M.toAscList problem.nodes
        ]
  where
    defaults = map (, S.empty) $ [Ret, Err] ++ map Addr (M.keys problem.nodes)
