module BV.Core.Types.Extras.Problem
    ( predsOf
    , varNamesOfProblem
    ) where

import BV.Core.Types
import BV.Core.Types.Extras.Program

import qualified Data.Map as M
import qualified Data.Set as S
import Optics

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
