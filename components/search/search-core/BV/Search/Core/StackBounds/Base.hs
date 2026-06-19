{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Search.Core.StackBounds.Base
    ( DiscoverStackBoundsInput (..)
    , FunTag (..)
    , callClosure
    , convertIdent
    , defaultVisit
    , functionCalls
    , makeCallGraph
    , recursionGroups
    , runGraphSlice
    ) where

import BV.Search.Core.GraphSlice

import BV.Core.Arch (archPtrSizeBytes)
import BV.Core.Logic (splitScalarPairs)
import BV.Core.Types
import BV.Core.Types.Extras.Expr (eqE, machineWordT, machineWordVarE, memAccE,
                                  memT, numE, plusE, varE)
import BV.Core.Types.Extras.Problem
import BV.Core.Types.Extras.Program (FunctionSignature (..),
                                     signatureOfFunction)
import BV.Core.Types.Extras.ProofCheck (numberVC, offsetVC)
import BV.Utils

import Control.DeepSeq (NFData)
import qualified Data.Array as A
import Data.Foldable (toList)
import Data.Function (applyWhen)
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as G
import Data.List (findIndex, genericIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics
import Text.Printf (PrintfArg (formatArg))

data DiscoverStackBoundsInput
  = DiscoverStackBoundsInput
      { structs :: ByTag' (M.Map Ident Struct)
      , rodata :: ROData
      , lookupFunction :: WithTag' Ident -> Function
      , pairingIds :: S.Set PairingId'
      , includeAsmFrom :: S.Set Ident
      }
  deriving (Generic)

newtype FunTag (t :: AsmRefineTag)
  = FunTag Int
  deriving (Enum, Eq, Generic, NFData, Ord, Show)

instance PrintfArg (FunTag t) where
    formatArg = formatArgSimple prettyTag

instance Tag (FunTag t) where
    prettyTag (FunTag i) = "fun" ++ show i
    parsePrettyTag = unimplemented

instance KnownAsmRefineTag t => HasTagIsAsm (FunTag t) where
    tagIsAsm _ = tagIsAsm (asmRefineTagVal (Proxy @t))

--

data CallGraph
  = CallGraph
      { graph :: Graph
      , vertexToIdent :: Vertex -> Ident
      , identToVertex :: Ident -> Vertex
      }
  deriving (Generic)

type CallGraphEdges = [((), Ident, [Ident])]

makeCallGraphEdges :: M.Map Ident Function -> CallGraphEdges
makeCallGraphEdges functions =
    [ ((), k, toList (functionCalls v))
    | (k, v) <- M.toList functions
    ]

makeCallGraphFromEdges :: CallGraphEdges -> CallGraph
makeCallGraphFromEdges edges =
    CallGraph
        { graph
        , vertexToIdent = view _2 . vertexToIdent'
        , identToVertex = fromJust . identToVertex'
        }
  where
    (graph, vertexToIdent', identToVertex') = G.graphFromEdges edges

makeCallGraph :: M.Map Ident Function -> CallGraph
makeCallGraph = makeCallGraphFromEdges . makeCallGraphEdges

callClosure :: (Ident -> Function) -> S.Set Ident -> S.Set Ident
callClosure lookupFun = genericClosure $ \f -> functionCalls (lookupFun f)

genericClosure :: Ord a => (a -> S.Set a) -> S.Set a -> S.Set a
genericClosure neighbors = go S.empty
  where
    go visited toVisit = case S.minView toVisit of
        Nothing -> visited
        Just (cur, rest) ->
            let visited' = S.insert cur visited
                toVisit' = rest <> S.difference (neighbors cur) visited'
             in go visited' toVisit'

functionCalls :: Function -> S.Set Ident
functionCalls fun = S.fromList $ fun ^.. #body % _Just % #nodes % folded % #_NodeCall % #functionName

recursionGroups :: CallGraph -> [(S.Set Ident, S.Set Ident)]
recursionGroups g =
    [ let comp' = S.map g.vertexToIdent comp
          prevs = S.fromList
                    [ g.vertexToIdent v
                    | v <- G.vertices g.graph
                    , v `S.notMember` comp
                    , any (`elem` (g.graph A.! v)) (S.toList comp)
                    ]
       in (comp', prevs)
    | comp <- map (S.fromList . toList) $ G.scc g.graph
    , let callsSelf v = v `elem` (g.graph A.! v)
       in S.size comp > 1 || callsSelf (S.findMin comp)
    ]

--

convertIdent :: FunctionSignature -> GraphExpr -> GraphExpr
convertIdent cSig ident = case ident.value of
    ExprValueOp OpTrue [] -> ident
    ExprValueOp OpEquals [arg, val] -> case arg.value of
        ExprValueVar name ->
            let Just argIndex = flip findIndex cSig.input $ \sigArg -> sigArg.name == name
             in (asmArgSeq cSig !! argIndex) `eqE` val

asmArgSeq :: FunctionSignature -> [GraphExpr]
asmArgSeq cSig = take numCArgs $ regArgSeq ++ stackArgSeq
  where
    r i = machineWordVarE (Ident ("r" ++ show (i :: Integer)))
    sp = r 13
    stack = varE memT "stack"
    (cArgVars, _, _) = splitScalarPairs cSig.input
    (cRetVars, _, _) = splitScalarPairs cSig.output
    numCArgs = length cArgVars
    numCRets = length cRetVars
    multiRet = numCRets > 1
    regArgSeq = map r $ applyWhen multiRet (drop 1) [0..3]
    stackArgSeq =
        [ let addr = sp `plusE` numE sp.ty (archPtrSizeBytes * i)
           in memAccE machineWordT addr stack
        | i <- [0..]
        ]

defaultVisit :: LoopData -> NodeId -> Visit
defaultVisit loopData n = Visit n (general ++ specific)
  where
    headOpt = case n of
        Addr addr -> loopHeadOf addr loopData
        _ -> Nothing
    general =
        [ Restr h (numberVC 0 <> offsetVC 1)
        | h <- loopHeadsOf loopData
        , Just h /= headOpt
        ]
    specific =
        [ Restr h (offsetVC 1)
        | Just h <- return headOpt
        ]

runGraphSlice
    :: DiscoverStackBoundsInput
    -> (forall t n a. (HasTagIsAsm t, MonadGraphSliceSendSExpr n) => AsmRefineTag -> Problem t -> GraphSliceT t n a -> n a)
runGraphSlice input tag problem m = runGraphSliceT hooks repGraphInput m
  where
    lookupSig (WithTag _ name) = signatureOfFunction $ input.lookupFunction $ WithTag tag name
    argRenames =
        problemArgRenames problem $
            lookupSig <$>
                withTags (pairingIdOfProblem problem)
    repGraphInput = GraphSliceInput
        { structs = byTagFromN (length problem.sides) $ const $ viewAtTag tag input.structs
        , rodata = input.rodata
        , problem
        }
    hooks = (if tag == Asm then withAsmHooks else id) $ withFast defaultGraphSliceHooks
    withAsmHooks =
        withAsmStackSplitting lookupSig argRenames .
            withConstRetAssumptions constRets
    constRets callee i =
        let sig = lookupSig callee
            out = sig.output `genericIndex` i
         in
            if tagIsAsm callee.tag && out.name `S.member` calleeSavedNames
            then Just (toInteger (fromJust (findIndex (== out) sig.input)))
            else Nothing
    r i = Ident $ "r" ++ show (i :: Integer)
    calleeSavedNames = S.fromList $ map r [4..12] ++ [r 13, "dom", "dom_stack"]
