module BV.Core.Types.Extras.Problem
    ( ArgRenames
    , argRenamesOf
    , pairingIdOfProblem
    , predsOf
    , varNamesOfProblem
    ) where

import BV.Core.Graph
import BV.Core.Types
import BV.Core.Types.Extras.Program
import BV.Utils

import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Optics

type ArgRenames t = PairingEqSideQuadrant t -> Ident -> Ident

argRenamesOf :: Tag t => (WithTag t Ident -> Function) -> Problem t -> ArgRenames t
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

pairingIdOfProblem :: Problem t -> PairingId t
pairingIdOfProblem problem = view #name <$> problem.sides

varNamesOfProblem :: Tag t => Traversal' (Problem t) Ident
varNamesOfProblem =
    (#sides % traversed % (#input `adjoin` #output) % traversed % varNamesOf)
        `adjoin` (#nodes % traversed % varNamesOf)

predsOf :: Problem t -> NodeGraph -> ByTag t (M.Map NodeId (S.Set NodeAddr))
predsOf problem g = problem.sides <&> \side ->
    let nodes = S.fromList $ Ret : Err : side.entryPoint : reachableFrom g side.entryPoint
     in M.unionWith (<>) (M.fromSet (const S.empty) nodes) $ M.fromListWith (<>) $ concat
            [ [ (cont, S.singleton nodeAddr)
              | cont <- problem ^.. #nodes % at nodeAddr % unwrapped % nodeConts
              ]
            | Addr nodeAddr <- S.toList nodes
            ]
