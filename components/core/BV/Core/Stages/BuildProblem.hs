{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Stages.BuildProblem
    ( ProblemBuilder
    , addEntrypoint
    , addEntrypoints
    , buildProblem
    , doAnalysis
    , extractProblem
    , extractProblemWithAnalysis
    , initProblemBuilder
    , inline
    , inlineAtPoint
    , inlineEntryForPoint
    , patchNoreturnCallConts
    ) where

import BV.Core.GenerateFreshName
import BV.Core.Logic
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Utils (ensureM, expecting, expectingAt, expectingIx, unwrapped)

import Control.Monad (guard, unless)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (StateT, evalStateT, gets)
import Data.Foldable (for_, sequenceA_, toList, traverse_)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=), (<<%=))
import Data.Vector.Internal.Check (HasCallStack)

data ProblemBuilder t
  = ProblemBuilder
      { sides :: M.Map t ProblemBuilderSide
      , common :: ProblemBuilderCommon
      }
  deriving (Generic)

data ProblemBuilderCommon
  = ProblemBuilderCommon
      { nextAddr :: Integer
      , vars :: S.Set Ident
      }
  deriving (Generic)

data ProblemBuilderSide
  = ProblemBuilderSide
      { problem :: ProblemSide
      , meta :: ProblemBuilderSideMeta
      }
  deriving (Generic)

data ProblemBuilderSideMeta
  = ProblemBuilderSideMeta
      { byNode :: M.Map NodeAddr (NodeSource, Int)
      , bySource :: M.Map NodeSource [NodeAddr]
      }
  deriving (Generic)

initProblemBuilder :: ProblemBuilder t
initProblemBuilder = ProblemBuilder
    { sides = M.empty
    , common = ProblemBuilderCommon
        { nextAddr = 1 -- match graph-refine
        , vars = S.empty
        }
    }

initProblemSideMeta :: ProblemBuilderSideMeta
initProblemSideMeta = ProblemBuilderSideMeta
    { byNode = M.empty
    , bySource = M.empty
    }

extractProblem :: Tag t => ProblemBuilder t -> Problem t
extractProblem builder = Problem
    { sides = byTagFromN (M.size builder.sides) ((.problem) . (builder.sides M.!))
    }

extractProblemWithAnalysis :: Tag t => ProblemBuilder t -> ProblemWithAnalysis t
extractProblemWithAnalysis = augmentProblem . extractProblem

--

nodeAt :: Tag t => t -> NodeAddr -> Lens' (ProblemBuilder t) Node
nodeAt tag addr = #sides % expectingAt tag % #problem % #nodes % expectingAt addr

reserveNodeAddr :: Monad m => StateT (ProblemBuilder t) m NodeAddr
reserveNodeAddr = do
    addr <- #common % #nextAddr <<%= (+ 1)
    return $ NodeAddr addr

appendNode :: (Tag t, Monad m) => t -> Node -> StateT (ProblemBuilder t) m NodeAddr
appendNode tag node = do
    addr <- reserveNodeAddr
    #sides % expectingAt tag % #problem % #nodes %= M.insertWith undefined addr node
    return addr

getFreshName :: Monad m => Ident -> StateT (ProblemBuilder t) m Ident
getFreshName hint = zoom (#common % #vars) $ takeFreshNameWith Ident hint.unwrap

--

data AddFunctionRenames
  = AddFunctionRenames
      { addrs :: Map NodeAddr NodeAddr
      , vars :: Map Ident Ident
      }
  deriving (Generic)

addFunction
    :: Monad m
    => Named Function
    -> NodeId
    -> StateT
        (ProblemBuilder t)
        m
        (AddFunctionRenames, NodeMap, ProblemBuilderSideMeta -> ProblemBuilderSideMeta)
addFunction (Named funName fun) retTarget = do
    renames <- do
        nodeAddrRenames <- M.fromList <$>
            for (toList origNodeAddrs) (\addr -> (addr,) <$> reserveNodeAddr)
        varRenames <- M.fromList <$>
            for (toList origVars) (\name -> (name,) <$> getFreshName name)
        return $ AddFunctionRenames
            { addrs = nodeAddrRenames
            , vars = varRenames
            }
    let renameVar = (renames.vars !)
        renameNodeAddr = (renames.addrs !)
        renameNodeId = \case
            Ret -> retTarget
            Err -> Err
            Addr addr -> Addr (renameNodeAddr addr)
        renameNode = (varNamesOf %~ renameVar) . (nodeConts %~ renameNodeId)
        nodes = M.fromList
            [ (renameNodeAddr origAddr, renameNode (funBody.nodes ! origAddr))
            | origAddr <- toList origNodeAddrs
            ]
        addMeta = foldl (flip (.)) id
            [ insertNodeMeta (renameNodeAddr origAddr) (NodeSource funName origAddr)
            | origAddr <- toList origNodeAddrs
            ]
    return (renames, nodes, addMeta)
  where
    funBody = fun ^. #body % unwrapped
    funGraph = makeNodeGraph funBody.nodes
    origNodeAddrs = S.fromList $ reachableFrom funGraph funBody.entryPoint ^.. traversed % #_Addr
    origVars = S.fromList $ fun ^.. varDeclsOf % #name

insertNodeMeta :: NodeAddr -> NodeSource -> ProblemBuilderSideMeta -> ProblemBuilderSideMeta
insertNodeMeta addr nodeSource meta = ProblemBuilderSideMeta
    { bySource
    , byNode
    }
  where
    (indexInProblem, bySource) =
        let f curOpt =
                let cur = fromMaybe [] curOpt
                 in (length cur, Just (cur ++ [addr]))
         in M.alterF f nodeSource meta.bySource
    byNode = M.insertWith undefined addr (nodeSource, indexInProblem) meta.byNode

--

buildProblem :: Tag t => (WithTag t Ident -> Function) -> InlineScript t -> ByTag t (Named Function) -> Problem t
buildProblem lookupFun inlineScript funs = runIdentity . flip evalStateT initProblemBuilder $ do
    addEntrypoints funs
    doAnalysis
    for_ inlineScript $ \entry -> do
        inline lookupFun entry
        doAnalysis
    padMergePoints
    doAnalysis
    gets extractProblem

addEntrypoints :: (Tag t, Monad m) => ByTag t (Named Function) -> StateT (ProblemBuilder t) m ()
addEntrypoints funs = for_ (withTags funs) $ \fun -> addEntrypoint fun

addEntrypoint :: (Tag t, Monad m) => WithTag t (Named Function) -> StateT (ProblemBuilder t) m ()
addEntrypoint (WithTag tag namedFun@(Named name fun)) = do
    (renames, nodes, addMeta) <- addFunction namedFun Ret
    let renameArgs = traversed % #name %~ (renames.vars !)
        newSide = ProblemBuilderSide
            { problem = ProblemSide
                { name = name
                , input = renameArgs fun.input
                , output = renameArgs fun.output
                , entryPoint = (fromJust fun.body).entryPoint & #_Addr %~ (renames.addrs !)
                , nodes
                }
            , meta = addMeta initProblemSideMeta
            }
    #sides %= M.insertWith undefined tag newSide

--

doAnalysis :: (Tag t, Monad m) => StateT (ProblemBuilder t) m ()
doAnalysis = do
    forceSimpleLoopReturns

-- TODO apply to inner loops too
forceSimpleLoopReturns :: (Tag t, Monad m) => StateT (ProblemBuilder t) m ()
forceSimpleLoopReturns = doBySide $ \(WithTag tag swa) ->
    for_ swa.analysis.loopData.outermostLoops $ \loop -> do
        let rets = S.toList $ S.filter (`S.member` loop.members) $ swa.analysis.preds (Addr loop.head)
        let alreadySimple = [ isNodeNoop (swa.problem.nodes ! ret) | ret <- rets ] == [True]
        unless alreadySimple $ do
            simpleRetNodeAddr <- appendNode tag $ trivialNode (Addr loop.head)
            for_ rets $ \ret -> replaceCont tag ret loop.head simpleRetNodeAddr

padMergePoints :: (Tag t, Monad m) => StateT (ProblemBuilder t) m ()
padMergePoints = doBySide $ \(WithTag tag swa) -> sequenceA_ $ do
    addr <- M.keys swa.problem.nodes
    let preds = swa.analysis.preds (Addr addr)
    guard $ S.size preds > 1
    predAddr <- S.toList preds
    let predNode = swa.problem.nodes ! predAddr
    guard $ predNode /= trivialNode (Addr addr)
    return $ do
        paddingNodeAddr <- appendNode tag $ NodeBasic $ BasicNode
            { next = Addr addr
            , varUpdates = []
            }
        replaceCont tag predAddr addr paddingNodeAddr

doBySide
    :: (Tag t, Monad m)
    => (WithTag t ProblemSideWithAnalysis -> StateT (ProblemBuilder t) m ())
    -> StateT (ProblemBuilder t) m ()
doBySide f = do
    pwa <- gets extractProblemWithAnalysis
    traverse_ f (withTags (problemSidesWithAnalysis pwa))

replaceCont
    :: (Tag t, Monad m)
    => t
    -> NodeAddr
    -> NodeAddr
    -> NodeAddr
    -> StateT (ProblemBuilder t) m ()
replaceCont tag addr origContAddr newContAddr =
    modifying (nodeAt tag addr % nodeConts % #_Addr) $ \contAddr ->
        if contAddr == origContAddr
        then newContAddr
        else contAddr

--

inline :: (Tag t, Monad m) => (WithTag t Ident -> Function) -> InlineScriptEntry t -> StateT (ProblemBuilder t) m ()
inline lookupFun entry = do
    addr <- use $
        #sides % expectingAt entry.tag % #meta %
            #bySource % expectingAt entry.nodeSource % expectingIx entry.indexInProblem
    inlineInner lookupFun addr entry

inlineEntryForPoint
    :: HasCallStack => (Tag t, Monad m) => WithTag t NodeAddr -> StateT (ProblemBuilder t) m (InlineScriptEntry t)
inlineEntryForPoint (WithTag tag addr) = do
    (nodeSource, indexInProblem) <- use $
        #sides % expectingAt tag % #meta %
            #byNode % expectingAt addr
    inlinedFunctionName <- use $ nodeAt tag addr % expecting #_NodeCall % #functionName
    return $ InlineScriptEntry
            { tag
            , nodeSource
            , indexInProblem
            , inlinedFunctionName
            }

inlineAtPoint
    :: HasCallStack => (Tag t, Monad m) => (WithTag t Ident -> Function) -> (WithTag t NodeAddr) -> StateT (ProblemBuilder t) m (InlineScriptEntry t)
inlineAtPoint lookupFun (WithTag tag addr) = do
    entry <- inlineEntryForPoint (WithTag tag addr)
    inlineInner lookupFun addr entry
    return entry

inlineInner
    :: HasCallStack => (Tag t, Monad m)
    => (WithTag t Ident -> Function)
    -> NodeAddr
    -> InlineScriptEntry t
    -> StateT (ProblemBuilder t) m ()
inlineInner lookupFun addr entry = do
    callNode <- use $ nodeAt entry.tag addr % expecting #_NodeCall
    ensureM $ callNode.functionName == entry.inlinedFunctionName
    let fun = lookupFun (WithTag entry.tag callNode.functionName)
    exitNodeAddr <- reserveNodeAddr
    (renames, newNodes, addMeta) <- addFunction (Named callNode.functionName fun) (Addr exitNodeAddr)
    let entryNodeAddr = renames.addrs ! (fun ^. #body % unwrapped % #entryPoint % expecting #_Addr)
    let entryNode = NodeBasic $ BasicNode
            { next = Addr entryNodeAddr
            , varUpdates =
                [ VarUpdate
                    { var = NameTy
                        { name = renames.vars ! arg.name
                        , ty = arg.ty
                        }
                    , val = callInput
                    }
                | (arg, callInput) <- zip fun.input callNode.input
                ]
            }
    let exitNode = NodeBasic $ BasicNode
            { next = callNode.next
            , varUpdates =
                [ VarUpdate
                    { var = NameTy
                        { name = callOutput.name
                        , ty = arg.ty
                        }
                    , val = varE arg.ty (renames.vars ! arg.name)
                    }
                | (arg, callOutput) <- zip fun.output callNode.output
                ]
            }
    zoom (#sides % expectingAt entry.tag) $ do
        modifying #meta addMeta
        modifying (#problem % #nodes) $
            M.unionWith undefined newNodes .
            M.insert addr entryNode .
            M.insertWith undefined exitNodeAddr exitNode

--
-- TODO move?

patchNoreturnCallConts :: (Tag t, Monad m) => (Ident -> Bool) -> StateT (ProblemBuilder t) m ()
patchNoreturnCallConts isNoreturn = doBySide $ \(WithTag tag _) -> do
    modifying (#sides % expectingAt tag % #problem % #nodes % traversed % #_NodeCall) $ \callNode ->
        callNode & #next %~ (if isNoreturn callNode.functionName then const Err else id)
    pruneUnreachableNodes

pruneUnreachableNodes :: (Tag t, Monad m) => StateT (ProblemBuilder t) m ()
pruneUnreachableNodes = doBySide $ \(WithTag tag swa) -> do
    let reachable = S.fromList $ reachableFrom swa.analysis.nodeGraph swa.problem.entryPoint
    modifying (#sides % expectingAt tag % #problem % #nodes) $ M.filterWithKey
        (\addr _ -> Addr addr `S.member` reachable)
