{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Types.Extras.Problem
    ( ArgRenames
    , LoopDataMap
    , NodeGraph
    , ProblemAnalysis (..)
    , ProblemWithAnalysis (..)
    , analyzeProblem
    , augmentProblem
    , computePreds
    , createLoopDataMap
    , isReachableFrom
    , loopBodyInnerLoops
    , loopBodyOf
    , loopHeadOf
    , loopHeadsFrom
    , loopHeadsIncludingInner
    , loopHeadsOf
    , makeNodeGraph
    , nodeTagMap
    , pairingIdOfProblem
    , problemArgRenames
    , reachableFrom
    , varNamesOfProblem
    ) where

import BV.Core.Types
import BV.Core.Types.Extras.Program
import BV.Utils

import Data.Foldable (toList)
import Data.Function (applyWhen)
import Data.Functor (void)
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as G
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

type ArgRenames t = PairingEqSideQuadrant t -> Ident -> Ident

problemArgRenames :: Tag t => Problem t -> ByTag t FunctionSignature -> ArgRenames t
problemArgRenames problem sigs quadrant mangledName =
    fromJust $ lookup mangledName (zip (map (.name) mangledArgs) (map (.name) origArgs))
  where
    sig = viewAtTag quadrant.tag sigs
    origArgs = case quadrant.direction of
        PairingEqDirectionIn -> sig.input
        PairingEqDirectionOut -> sig.output
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

--

computePreds :: Problem t -> NodeGraph -> ByTag t (M.Map NodeId (S.Set NodeAddr))
computePreds problem g = problem.sides <&> \side ->
    let nodes = S.fromList $ Ret : Err : side.entryPoint : reachableFrom g side.entryPoint
     in M.unionWith (<>) (M.fromSet (const S.empty) nodes) $ M.fromListWith (<>) $ concat
            [ [ (cont, S.singleton nodeAddr)
              | cont <- problem ^.. #nodes % at nodeAddr % unwrapped % nodeConts
              ]
            | Addr nodeAddr <- S.toList nodes
            ]

-- TODO more efficient but more opaque
computePreds' :: Tag t => Problem t -> (NodeAddr -> t) -> ByTag t (NodeId -> S.Set NodeAddr)
computePreds' problem nodeTag = withTags (void problem.sides) <&> \(WithTag tag ()) nodeId ->
    let f = applyWhen (not (is #_Addr nodeId)) (S.filter (\pred_ -> nodeTag pred_ == tag))
     in f $ clobbered M.! nodeId
  where
    clobbered = M.fromListWith (<>) $ concat
        [ [ (cont, S.singleton nodeAddr)
          | cont <- node ^.. nodeConts
          ]
        | (nodeAddr, node) <- M.toList problem.nodes
        ]

data ProblemWithAnalysis t
  = ProblemWithAnalysis
      { problem :: Problem t
      , analysis :: ProblemAnalysis t
      }
  deriving (Generic)

data ProblemAnalysis t
  = ProblemAnalysis
      { nodeGraph :: NodeGraph
      , nodeTag :: NodeAddr -> t
      , loopData :: LoopDataMap
      , preds :: ByTag t (NodeId -> S.Set NodeAddr)
      , vars :: S.Set Ident
      }
  deriving (Generic)

analyzeProblem :: Tag t => Problem t -> ProblemAnalysis t
analyzeProblem problem = ProblemAnalysis
    { nodeGraph
    , nodeTag
    , loopData = createLoopDataMap problem nodeGraph
    , preds = (M.!) <$> computePreds problem nodeGraph
    -- , preds = computePreds' problem nodeTag
    , vars = S.fromList $ toListOf varNamesOfProblem problem
    }
  where
    nodeGraph = makeNodeGraph problem.nodes
    nodeTag = (M.!) $ nodeTagMap problem nodeGraph

augmentProblem :: Tag t => Problem t -> ProblemWithAnalysis t
augmentProblem problem = ProblemWithAnalysis
    { problem
    , analysis = analyzeProblem problem
    }

--

data NodeGraph
  = NodeGraph
      { graph :: Graph
      , nodeIdMap :: Vertex -> NodeId
      , nodeIdMapRev :: NodeId -> Maybe Vertex
      }
  deriving (Generic)

type NodeGraphEdges = [((), NodeId, [NodeId])]

makeNodeGraphEdges:: [(NodeAddr, Node)] -> NodeGraphEdges
makeNodeGraphEdges nodeMap =
      ((), Ret, [])
    : ((), Err, [])
    : (nodeMap <&> \(addr, node) -> ((), Addr addr, toListOf nodeConts node))

makeNodeGraphFromEdges :: NodeGraphEdges -> NodeGraph
makeNodeGraphFromEdges edges =
    NodeGraph
        { graph
        , nodeIdMap = view _2 . nodeIdMap'
        , nodeIdMapRev
        }
  where
    (graph, nodeIdMap', nodeIdMapRev) = G.graphFromEdges edges

makeNodeGraph :: NodeMap -> NodeGraph
makeNodeGraph = makeNodeGraphFromEdges . makeNodeGraphEdges . M.toList

-- Algorithms

reachableFrom :: NodeGraph -> NodeId -> [NodeId]
reachableFrom g from = map g.nodeIdMap $ G.reachable g.graph (fromJust (g.nodeIdMapRev from))

isReachableFrom :: NodeGraph -> NodeId -> NodeId -> Bool
isReachableFrom g from to_ = G.path g.graph (fromJust (g.nodeIdMapRev from)) (fromJust (g.nodeIdMapRev to_))

loopHeadsFromGeneric :: G.Graph -> [Vertex] -> [(Vertex, S.Set Vertex)]
loopHeadsFromGeneric g entryPoints = catMaybes
    [ findHead comp <&> (, comp)
    | comp <- sccs
    ]
  where
    sccs =
        [ comp
        | comp <- map (S.fromList . toList) (G.scc g)
        , S.size comp > 1
        ]
    findHead comp = find (`S.member` comp) inOrder
      where
        inOrder = foldMap toList $ G.dfs g entryPoints

loopHeadsFrom :: NodeGraph -> [NodeId] -> [(NodeAddr, S.Set NodeAddr)]
loopHeadsFrom g entryPoints =
    [ (toNodeAddr h, S.map toNodeAddr body)
    | (h, body) <- loopHeadsFromGeneric g.graph (map (fromJust . g.nodeIdMapRev) entryPoints)
    ]
  where
    toNodeAddr v = nodeAddrFromNodeId $ g.nodeIdMap v

nodeTagMap :: Tag t => Problem t -> NodeGraph -> M.Map NodeAddr t
nodeTagMap problem nodeGraph =
    M.fromListWith (error "unexpected") $ byTag ^.. folded % folded
  where
    byTag = withTags problem.sides <&> \(WithTag tag side) ->
        [ (addr, tag)
        | addr <- reachableFrom nodeGraph side.entryPoint ^.. folded % #_Addr
        ]

type LoopDataMap = M.Map NodeAddr LoopData

data LoopData
  = LoopHead (S.Set NodeAddr)
  | LoopMember NodeAddr
  deriving (Eq, Generic, Ord, Show)

createLoopDataMap :: Tag t => Problem t -> NodeGraph -> LoopDataMap
createLoopDataMap problem nodeGraph =
    M.fromList $ flip foldMap heads $ \(loopHead, scc) ->
        [(loopHead, LoopHead scc)]
            <> flip mapMaybe (S.toList scc)
                (\member -> if member == loopHead then Nothing else Just (member, LoopMember loopHead))
  where
    heads = loopHeadsFrom nodeGraph $ toListOf (folded % #entryPoint) problem.sides

loopHeadsOf :: LoopDataMap -> [NodeAddr]
loopHeadsOf loopDataMap = flip mapMaybe (M.toList loopDataMap) $ \(k, v) -> case v of
    LoopHead _ -> Just k
    LoopMember _ -> Nothing

loopHeadOf :: NodeAddr -> LoopDataMap -> Maybe NodeAddr
loopHeadOf n loopDataMap = M.lookup n loopDataMap <&> \case
    LoopHead _ -> n
    LoopMember n' -> n'

loopBodyOf :: NodeAddr -> LoopDataMap -> S.Set NodeAddr
loopBodyOf n loopDataMap =
    loopDataMap ^. expectingAt (fromJust (loopHeadOf n loopDataMap)) % expecting #_LoopHead

--

loopBodyInnerLoops :: NodeMap -> NodeAddr -> S.Set NodeAddr -> [(NodeAddr, S.Set NodeAddr)]
loopBodyInnerLoops nodes loopHead loopBody =
    loopHeadsFrom g $ nodes ^.. at loopHead % unwrapped % nodeConts
  where
    g = makeNodeGraph $ M.restrictKeys nodes $ S.delete loopHead loopBody

loopHeadsIncludingInner :: NodeMap -> LoopDataMap -> [NodeAddr]
loopHeadsIncludingInner nodes m =
    let heads = loopHeadsOf m
     in go heads [ (h, loopBodyOf h m ) | h <- heads ]
  where
    go heads [] = heads
    go heads ((h, body):check) =
        let comps = loopBodyInnerLoops nodes h body
         in go
                (heads ++ map fst comps)
                (check ++ comps)
