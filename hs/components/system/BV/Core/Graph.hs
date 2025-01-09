module BV.Core.Graph where

import Data.Graph (Graph, Vertex, graphFromEdges)
import Data.Map (Map)
import qualified Data.Map as M
import qualified GHC.Arr as A
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

makeNodeGraph :: Map NodeAddr Node -> NodeGraph
makeNodeGraph = makeNodeGraphTheirs

makeNodeGraphOurs :: Map NodeAddr Node -> NodeGraph
makeNodeGraphOurs nodeMap =
    NodeGraph
        { graph
        , nodeIdMap = (nodeIdMap' A.!)
        , nodeIdMapRev = (nodeIdMapRev' M.!?)
        }
  where
    vertices = zip [0 ..]
        ( (Ret, [])
        : (Err, [])
        : (M.assocs nodeMap <&> (_1 %~ Addr) . (_2 %~ (^.. nodeConts % to (nodeIdMapRev' M.!))))
        )
    (graphList, nodeIdMapList, nodeIdMapRevList) = unzip3
        [ (neighbors, nodeId, (nodeId, i))
        | (i, (nodeId, neighbors)) <- vertices
        ]
    bounds = (0, M.size nodeMap + 2)
    graph = A.listArray bounds graphList
    nodeIdMap' = A.listArray bounds nodeIdMapList
    nodeIdMapRev' = M.fromList nodeIdMapRevList

makeNodeGraphTheirs :: Map NodeAddr Node -> NodeGraph
makeNodeGraphTheirs nodeMap =
    NodeGraph
        { graph
        , nodeIdMap = view _2 . nodeIdMap'
        , nodeIdMapRev
        }
  where
    (graph, nodeIdMap', nodeIdMapRev) = graphFromEdges
        ( ((), Ret, [])
        : ((), Err, [])
        : (M.assocs nodeMap <&> \(addr, node) -> ((), Addr addr, toListOf nodeConts node))
        )
