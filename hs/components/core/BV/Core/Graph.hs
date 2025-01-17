{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Graph
    ( NodeGraph (..)
    , NodeGraphEdges
    , makeNodeGraph
    , nodeGraphEdgesWith
    , nodeGraphEdges
    , reachable
    ) where

import Data.Graph (Graph, Vertex)
import qualified Data.Graph as G
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Optics.Core

import BV.Core.Types

data NodeGraph
  = NodeGraph
      { graph :: Graph
      , nodeIdMap :: Vertex -> NodeId
      , nodeIdMapRev :: NodeId -> Maybe Vertex
      }
  deriving (Generic)

makeNodeGraph :: NodeGraphEdges -> NodeGraph
makeNodeGraph edges =
    NodeGraph
        { graph
        , nodeIdMap = view _2 . nodeIdMap'
        , nodeIdMapRev
        }
  where
    (graph, nodeIdMap', nodeIdMapRev) = G.graphFromEdges edges

reachable :: NodeGraph -> NodeId -> [NodeId]
reachable g from = map g.nodeIdMap $ G.reachable g.graph (fromJust (g.nodeIdMapRev from))

type NodeGraphEdges = [((), NodeId, [NodeId])]

nodeGraphEdgesWith :: (a -> Node) -> Map NodeAddr a -> NodeGraphEdges
nodeGraphEdgesWith f nodeMap =
      ((), Ret, [])
    : ((), Err, [])
    : (M.assocs nodeMap <&> \(addr, node) -> ((), Addr addr, toListOf nodeConts (f node)))

nodeGraphEdges :: NodeMap -> NodeGraphEdges
nodeGraphEdges = nodeGraphEdgesWith id

-- loopHeads ::
