{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Graph
    ( NodeGraph (..)
    , makeNodeGraph
    , makeNodeGraphWith
    , reachable
    , nodeGraphEdgesWith
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

makeNodeGraphWith :: (a -> Node) -> Map NodeAddr a -> NodeGraph
makeNodeGraphWith = makeNodeGraphWithTheirs

makeNodeGraph :: NodeMap -> NodeGraph
makeNodeGraph = makeNodeGraphWith id

reachable :: NodeGraph -> NodeId -> [NodeId]
reachable g from = map g.nodeIdMap $ G.reachable g.graph (fromJust (g.nodeIdMapRev from))

nodeGraphEdgesWith :: (a -> Node) -> Map NodeAddr a -> [((), NodeId, [NodeId])]
nodeGraphEdgesWith f nodeMap =
      ((), Ret, [])
    : ((), Err, [])
    : (M.assocs nodeMap <&> \(addr, node) -> ((), Addr addr, toListOf nodeConts (f node)))

makeNodeGraphWithTheirs ::  (a -> Node) -> Map NodeAddr a -> NodeGraph
makeNodeGraphWithTheirs f nodeMap =
    NodeGraph
        { graph
        , nodeIdMap = view _2 . nodeIdMap'
        , nodeIdMapRev
        }
  where
    (graph, nodeIdMap', nodeIdMapRev) = G.graphFromEdges (nodeGraphEdgesWith f nodeMap)
