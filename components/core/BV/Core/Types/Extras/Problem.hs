{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Types.Extras.Problem
    ( ArgRenames
    , Loop (..)
    , LoopData (..)
    , NodeGraph
    , ProblemAnalysis (..)
    , ProblemWithAnalysis (..)
    , allLoopsOf
    , analyzeProblem
    , analyzeProblemFromPartial
    , augmentProblem
    , hasInnerLoop
    , inlineScriptsEquivalent
    , innerLoopsOf
    , innermostLoopContaining
    , isReachableFrom
    , makeNodeGraph
    , makeProblemWithAnalysisLens
    , outermostLoopContaining
    , pairingIdOfProblem
    , problemArgRenames
    , reachableFrom
    , varNamesOfProblem
    ) where

import BV.Core.Types
import BV.Core.Types.Extras.Program
import BV.Utils

import Data.Foldable (toList)
import Data.Function (applyWhen, on)
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as G
import Data.List (find, sort)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

-- TODO don't abuse PairingEqSideQuadrant
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

data ProblemWithAnalysis t
  = ProblemWithAnalysis
      { problem :: Problem t
      , analysis :: ProblemAnalysis t
      }
  deriving (Generic)

-- TODO add problem as field
data ProblemAnalysis t
  = ProblemAnalysis
      { nodeGraph :: NodeGraph
      , nodeTag :: NodeAddr -> t
      , loopData :: LoopData
      , preds :: ByTag t (NodeId -> S.Set NodeAddr)
      , varNames :: S.Set Ident
      }
  deriving (Generic)

analyzeProblem :: Tag t => Problem t -> ProblemAnalysis t
analyzeProblem problem = ProblemAnalysis
    { nodeGraph
    , nodeTag
    , loopData = makeLoopData problem nodeGraph
    , preds = computePreds problem nodeTag
    , varNames = S.fromList $ toListOf varNamesOfProblem problem
    }
  where
    nodeGraph = makeNodeGraph problem.nodes
    nodeTag = (M.!) $ nodeTagMap problem nodeGraph

analyzeProblemFromPartial :: Tag t => (NodeAddr -> t) -> S.Set Ident -> Problem t -> ProblemAnalysis t
analyzeProblemFromPartial nodeTag varNames problem = ProblemAnalysis
    { nodeGraph
    , nodeTag
    , loopData = makeLoopData problem nodeGraph
    , preds = computePreds problem nodeTag
    , varNames
    }
  where
    nodeGraph = makeNodeGraph problem.nodes

augmentProblem :: Tag t => Problem t -> ProblemWithAnalysis t
augmentProblem problem = ProblemWithAnalysis
    { problem
    , analysis = analyzeProblem problem
    }

makeProblemWithAnalysisLens :: Lens' s (Problem t) -> Lens' s (ProblemAnalysis t) -> Lens' s (ProblemWithAnalysis t)
makeProblemWithAnalysisLens p pa = lens
    (\s -> ProblemWithAnalysis (view p s) (view pa s))
    (\s c -> set p c.problem (set pa c.analysis s))

--

data NodeGraph
  = NodeGraph
      { graph :: Graph
      , vertexToNodeId :: Vertex -> NodeId
      , nodeIdToVertex :: NodeId -> Vertex
      }
  deriving (Generic)

type NodeGraphEdges = [((), NodeId, [NodeId])]

makeNodeGraphEdges :: NodeMap -> NodeGraphEdges
makeNodeGraphEdges nodeMap =
      ((), Ret, [])
    : ((), Err, [])
    : (M.toList nodeMap <&> \(addr, node) -> ((), Addr addr, toListOf nodeConts node))

makeNodeGraphFromEdges :: NodeGraphEdges -> NodeGraph
makeNodeGraphFromEdges edges =
    NodeGraph
        { graph
        , vertexToNodeId = view _2 . vertexToNodeId'
        , nodeIdToVertex = fromJust . nodeIdToVertex'
        }
  where
    (graph, vertexToNodeId', nodeIdToVertex') = G.graphFromEdges edges

makeNodeGraph :: NodeMap -> NodeGraph
makeNodeGraph = makeNodeGraphFromEdges . makeNodeGraphEdges

--

reachableFrom :: NodeGraph -> NodeId -> [NodeId]
reachableFrom g from = map g.vertexToNodeId $ G.reachable g.graph (g.nodeIdToVertex from)

isReachableFrom :: NodeGraph -> NodeId -> NodeId -> Bool
isReachableFrom g from to_ = G.path g.graph (g.nodeIdToVertex from) (g.nodeIdToVertex to_)

--

nodeTagMap :: Tag t => Problem t -> NodeGraph -> M.Map NodeAddr t
nodeTagMap problem nodeGraph =
    M.fromListWith undefined $ byTag ^.. folded % folded
  where
    byTag = withTags problem.sides <&> \(WithTag tag side) ->
        [ (addr, tag)
        | addr <- reachableFrom nodeGraph side.entryPoint ^.. folded % #_Addr
        ]

--

loopsFromGeneric :: G.Graph -> [Vertex] -> [(Vertex, S.Set Vertex)]
loopsFromGeneric g entryPoints =
    [ (h, body)
    | scc <- G.scc g
    , let body = S.fromList (toList scc)
    , S.size body > 1
    , let Just h = find (`S.member` body) inOrder
    ]
  where
    inOrder = foldMap toList $ G.dfs g entryPoints

data LoopData
  = LoopData
      { outermostLoops :: [Loop]
      , heads :: M.Map NodeAddr Loop
      , members :: M.Map NodeAddr Loop
      }
  deriving (Eq, Generic, Ord, Show)

data Loop
  = Loop
      { head :: NodeAddr
      , body :: S.Set NodeAddr
      , parent :: Maybe Loop
      , children :: [Loop]
      }
  deriving (Eq, Generic, Ord, Show)

makeLoopData :: Tag t => Problem t -> NodeGraph -> LoopData
makeLoopData problem nodeGraph = LoopData
    { outermostLoops
    , heads = M.fromList $ allLoops <&> \loop -> (loop.head, loop)
    , members = M.fromList $ concat $ allLoops <&> \loop ->
        [ (n, loop)
        | n <- S.toList loop.body
        ]
    }
  where
    outermostLoops = go nodeGraph Nothing $ toListOf (folded % #entryPoint) problem.sides
    allLoops = flattenLoops outermostLoops
    go g parent entryPoints = sort
        [ let toNodeAddr = nodeAddrOf . g.vertexToNodeId
              loop = Loop
                { head = toNodeAddr h
                , body = S.map toNodeAddr body
                , parent
                , children =
                    let g' = makeNodeGraphFromEdges
                            [ ((), Addr src, dsts)
                            | src <- S.toList loop.body
                            , let dsts =
                                    [ Addr dst
                                    | Addr dst <- problem.nodes ^.. at src % unwrapped % nodeConts
                                    , dst `S.member` loop.body
                                    , dst /= loop.head
                                    ]
                            ]
                     in go g' (Just loop) [Addr loop.head]
                }
          in loop
        | (h, body) <- loopsFromGeneric g.graph (map g.nodeIdToVertex entryPoints)
        ]

flattenLoops :: [Loop] -> [Loop]
flattenLoops loops =
    loops ++
        concat
            [ flattenLoops loop.children
            | loop <- loops
            ]

allLoopsOf :: LoopData -> [Loop]
allLoopsOf d = flattenLoops d.outermostLoops

innermostLoopContaining :: LoopData -> NodeAddr -> Maybe Loop
innermostLoopContaining d n = M.lookup n d.members

outermostLoopContaining :: LoopData -> NodeAddr -> Maybe Loop
outermostLoopContaining d n = go <$> innermostLoopContaining d n
  where
    go loop = maybe loop go loop.parent

--

-- TODO remove
hasInnerLoop :: Loop -> Bool
hasInnerLoop loop = not (null (innerLoopsOf loop))

innerLoopsOf :: Loop -> [Loop]
innerLoopsOf loop = loop.children

--

computePreds :: Tag t => Problem t -> (NodeAddr -> t) -> ByTag t (NodeId -> S.Set NodeAddr)
computePreds problem nodeTag = withTags problem.sides <&> \(WithTag tag _) nodeId ->
    applyWhen
        (not (is #_Addr nodeId))
        (S.filter ((==) tag . nodeTag))
        (M.findWithDefault S.empty nodeId clobbered)
  where
    clobbered = M.fromListWith (<>) $ concat
        [ [ (cont, S.singleton nodeAddr)
          | cont <- node ^.. nodeConts
          ]
        | (nodeAddr, node) <- M.toList problem.nodes
        ]

---

inlineScriptsEquivalent :: Tag t => InlineScript t -> InlineScript t -> Bool
inlineScriptsEquivalent = (==) `on` f
  where
    f script =
        let s = S.fromList script
         in ensure (S.size s == length script) s
