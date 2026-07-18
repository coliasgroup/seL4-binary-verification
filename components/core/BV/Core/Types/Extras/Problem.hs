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
    , inlineScriptsEquivalent
    , innermostLoopContaining
    , isNonTriviallyReachableFrom
    , isReachableFrom
    , isSyntacticConstant
    , loopIsComplex
    , loopIsSimple
    , makeNodeGraph
    , makeProblemWithAnalysisLens
    , outermostLoopContaining
    , pairingIdOfProblem
    , problemArgRenames
    , reachableFrom
    , varNamesOfProblem
    ) where

import BV.Core.Types
import BV.Core.Types.Extras.Expr (varFromNameTyE)
import BV.Core.Types.Extras.Program
import BV.Utils

import Control.Monad (unless, when)
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.State (evalStateT, get, modify)
import Data.Either (isRight)
import Data.Foldable (for_, toList)
import Data.Function (applyWhen, on)
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as G
import Data.List (elemIndex, find, genericIndex, sortOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
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

-- TODO rename to AnalyzedProblem
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
      , isNonTriviallyReachableFrom :: NodeAddr -> NodeId -> Bool
      }
  deriving (Generic)

analyzeProblem :: Tag t => Problem t -> ProblemAnalysis t
analyzeProblem problem = ProblemAnalysis
    { nodeGraph
    , nodeTag
    , loopData
    , preds = computePreds problem nodeTag
    , varNames = S.fromList $ toListOf varNamesOfProblem problem
    , isNonTriviallyReachableFrom = makeIsNonTriviallyReachableFrom problem nodeGraph loopData
    }
  where
    nodeGraph = makeNodeGraph problem.nodes
    nodeTag = (M.!) $ nodeTagMap problem nodeGraph
    loopData = makeLoopData problem nodeGraph

analyzeProblemFromPartial :: Tag t => (NodeAddr -> t) -> S.Set Ident -> Problem t -> ProblemAnalysis t
analyzeProblemFromPartial nodeTag varNames problem = ProblemAnalysis
    { nodeGraph
    , nodeTag
    , loopData
    , preds = computePreds problem nodeTag
    , varNames
    , isNonTriviallyReachableFrom = makeIsNonTriviallyReachableFrom problem nodeGraph loopData
    }
  where
    nodeGraph = makeNodeGraph problem.nodes
    loopData = makeLoopData problem nodeGraph

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

isNonTriviallyReachableFrom :: ProblemWithAnalysis t -> NodeAddr -> NodeId -> Bool
isNonTriviallyReachableFrom p from to_ =
    if Addr from /= to_
    then isReachableFrom p.analysis.nodeGraph (Addr from) to_
    else from `M.member` p.analysis.loopData.byMember

makeIsNonTriviallyReachableFrom :: Problem t -> NodeGraph -> LoopData -> NodeAddr -> NodeId -> Bool
makeIsNonTriviallyReachableFrom problem g loopData = \from to_ -> to_ `S.member` (m M.! from)
  where
    m = M.mapWithKey f problem.nodes
    f from _ = applyWhen (not keepSelf) (S.delete n) s
      where
        n = Addr from
        s = S.fromList $ reachableFrom g n
        keepSelf = from `M.member` loopData.byMember

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
      , byHead :: M.Map NodeAddr Loop
      , byMember :: M.Map NodeAddr Loop
      }
  deriving (Generic)

data Loop
  = Loop
      { head :: NodeAddr
      , members :: S.Set NodeAddr
      , parent :: Maybe Loop
      , children :: [Loop]
      }
  deriving (Generic)

makeLoopData :: Tag t => Problem t -> NodeGraph -> LoopData
makeLoopData problem nodeGraph = LoopData
    { outermostLoops
    , byHead = M.fromList $ allLoops <&> \loop -> (loop.head, loop)
    , byMember = M.fromList $ concat $ allLoops <&> \loop ->
        [ (n, loop)
        | n <- S.toList loop.members
        ]
    }
  where
    outermostLoops = go nodeGraph Nothing $ toListOf (folded % #entryPoint) problem.sides
    allLoops = flattenLoops outermostLoops
    go g parent entryPoints = sortOn (.head)
        [ let toNodeAddr = nodeAddrOf . g.vertexToNodeId
              loop = Loop
                { head = toNodeAddr h
                , members = S.map toNodeAddr body
                , parent
                , children =
                    let g' = makeNodeGraphFromEdges
                            [ ((), Addr src, dsts)
                            | src <- S.toList loop.members
                            , let dsts =
                                    [ Addr dst
                                    | Addr dst <- problem.nodes ^.. at src % unwrapped % nodeConts
                                    , dst `S.member` loop.members
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
innermostLoopContaining d n = M.lookup n d.byMember

outermostLoopContaining :: LoopData -> NodeAddr -> Maybe Loop
outermostLoopContaining d n = go <$> innermostLoopContaining d n
  where
    go loop = maybe loop go loop.parent

loopIsSimple :: Loop -> Bool
loopIsSimple loop = null loop.children

loopIsComplex :: Loop -> Bool
loopIsComplex = not . loopIsSimple

--

isSyntacticConstant
    :: Tag t
    => ProblemWithAnalysis t
    -> (WithTag t Ident -> Integer -> Maybe Integer)
    -> t
    -> NameTy
    -> Loop
    -> NodeAddr
    -> Bool
isSyntacticConstant p constRetAssumptions tag var loop split =
    ensure (not (loopIsComplex loop)) $
    isRight $
        runExcept
            (evalStateT
                (go (var.name, split))
                (S.singleton (var.name, split)))
  where
    go (name, addr) = do
        let localVar = NameTy name var.ty
        let node = p.problem.nodes M.! addr
        predName <- fromMaybe name <$> case node of
            NodeCall callNode -> do
                let isConst = case elemIndex localVar callNode.output of
                        Nothing -> True
                        Just outputIx -> case constRetAssumptions (WithTag tag callNode.functionName) (toInteger outputIx) of
                            Nothing -> False
                            Just intputIx -> callNode.input `genericIndex` intputIx == varFromNameTyE localVar
                if isConst
                    then return Nothing
                    else throwNotConst
            NodeBasic basicNode -> do
                let updateExprs =
                        [ u.val
                        | u <- basicNode.varUpdates
                        , u.var == localVar
                        ]
                case updateExprs of
                    [] -> return Nothing
                    [Expr _ (ExprValueVar ident)] -> return $ Just ident
                    [_] -> throwNotConst
                    _ -> error "unexpected"
            _ -> return Nothing
        let preds = S.intersection loop.members $ (viewAtTag tag p.analysis.preds) (Addr addr)
        for_ preds $ \predAddr -> do
            let predVar = (predName, predAddr)
            safe <- get
            unless (predVar `S.member` safe) $ do
                when (predAddr == split) throwNotConst
                go predVar
                modify $ S.insert predVar
    throwNotConst = throwError ()

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
