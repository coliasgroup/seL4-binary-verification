{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Graph
    ( NodeGraph (..)
    , makeNodeGraphWith
    , makeNodeGraph
    , reachable
    ) where

import Data.Graph (Graph, Vertex)
import qualified Data.Graph as G
import Data.Map (Map)
import qualified Data.Map as M
import qualified GHC.Arr as A
import GHC.Generics (Generic)
import Optics.Core
import Data.Maybe (fromJust)

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

makeNodeGraphWithOurs ::  (a -> Node) -> Map NodeAddr a -> NodeGraph
makeNodeGraphWithOurs f nodeMap =
    NodeGraph
        { graph
        , nodeIdMap = (nodeIdMap' A.!)
        , nodeIdMapRev = (nodeIdMapRev' M.!?)
        }
  where
    vertices = zip [0 ..]
        ( (Ret, [])
        : (Err, [])
        : (M.assocs nodeMap <&> (_1 %~ Addr) . (_2 %~ (^.. to f % nodeConts % to (nodeIdMapRev' M.!))))
        )
    (graphList, nodeIdMapList, nodeIdMapRevList) = unzip3
        [ (neighbors, nodeId, (nodeId, i))
        | (i, (nodeId, neighbors)) <- vertices
        ]
    bounds = (0, M.size nodeMap + 2)
    graph = A.listArray bounds graphList
    nodeIdMap' = A.listArray bounds nodeIdMapList
    nodeIdMapRev' = M.fromList nodeIdMapRevList

makeNodeGraphWithTheirs ::  (a -> Node) -> Map NodeAddr a -> NodeGraph
makeNodeGraphWithTheirs f nodeMap =
    NodeGraph
        { graph
        , nodeIdMap = view _2 . nodeIdMap'
        , nodeIdMapRev
        }
  where
    (graph, nodeIdMap', nodeIdMapRev) = G.graphFromEdges
        ( ((), Ret, [])
        : ((), Err, [])
        : (M.assocs nodeMap <&> \(addr, node) -> ((), Addr addr, toListOf nodeConts (f node)))
        )
