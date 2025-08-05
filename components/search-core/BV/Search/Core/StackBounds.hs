{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Search.Core.StackBounds
    ( DiscoverStackBoundsInput (..)
    , discoverStackBounds
    ) where

import BV.Core.RepGraph
import BV.Core.Stages
import BV.Core.Types
import BV.Logging
import BV.Search.Core.Solver
import BV.Utils

import Control.DeepSeq (NFData)
import qualified Data.Array as A
import Data.Foldable (for_, toList)
import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

data DiscoverStackBoundsInput
  = DiscoverStackBoundsInput
      { rodata :: ROData
      , functions :: WithTag' Ident -> Function
      , pairings :: S.Set PairingId'
      , includeAsmFrom :: S.Set Ident
      }
  deriving (Generic)

discoverStackBounds
    :: forall m n. (Monad m, MonadRepGraphSolverInteract n, MonadLoggerWithContext m, MonadLoggerWithContext n)
    => (forall a. n a -> m a)
    -> DiscoverStackBoundsInput
    -> m StackBounds
discoverStackBounds run input = do
    let asmClosure = callClosure (input.functions . WithTag Asm) input.includeAsmFrom
    logInfo $ show ("asm closure", asmClosure ^.. folded % #unwrap)
    let cClosure = S.fromList $ input.pairings ^.. folded % filtered ((`S.member` asmClosure) . getAsm) % atTag C
    logInfo $ show ("c closure", cClosure ^.. folded % #unwrap)
    let cIdents = getRecursionIdents runRepGraphAsm $ M.fromSet (input.functions . WithTag C) cClosure
    for_ (M.toList cIdents) $ \(k, v) -> logInfo $ show ("c ident", k.unwrap, v)
    todo

  where

    runRepGraph :: forall t a. Tag t => ByTag t (WithTag' Ident) -> RepGraphBase t n a -> m a
    runRepGraph fnames m =
        let repGraphInput = RepGraphBaseInput
                { structs = fnames <&> const M.empty
                , rodata = input.rodata
                , problem = undefined
                -- , problem = buildProblemSimple $ fnames <&> \fname ->
                --     Named fname.value (input.functions fname)
                }
            in run (runRepGraphBase repGraphInput m)

    runRepGraphC :: forall t a. Tag t => ByTag t Ident -> RepGraphBase t n a -> m a
    runRepGraphC = runRepGraph . fmap (WithTag C)

    runRepGraphAsm :: forall t a. Tag t => ByTag t Ident -> RepGraphBase t n a -> m a
    runRepGraphAsm = runRepGraph . fmap (WithTag Asm)

newtype FunTag
  = FunTag Integer
  deriving (Enum, Eq, Generic, NFData, Ord, Show)

instance Tag FunTag where
    prettyTag (FunTag i) = "fun" ++ show i
    parsePrettyTag = unimplemented

functionCalls :: Function -> [Ident]
functionCalls = toListOf $ #body % _Just % #nodes % folded % #_NodeCall % #functionName

getRecursionIdents
    :: forall m n. (Monad m, MonadRepGraphSolverInteract n, MonadLoggerWithContext m, MonadLoggerWithContext n)
    => (forall t a. Tag t => ByTag t Ident -> RepGraphBase t n a -> m a)
    -> M.Map Ident Function
    -> M.Map Ident [Expr]
getRecursionIdents runRepGraph functions =
    foldMap (computeRecursionIdents . S.map fromVertex) scc
  where
    (g, fromVertex', _toVertex') = G.graphFromEdges
        [ ((), n, functionCalls fun)
        | (n, fun) <- M.toList functions
        ]
    fromVertex = view _2 . fromVertex'
    scc =
        [ comp
        | comp <- map (S.fromList . toList) (G.scc g)
        , S.size comp > 1 || callsSelf (S.findMin comp)
        ]
    callsSelf v = v `elem` (g A.! v)
    prevs comp =
        [ v
        | v <- G.vertices g
        , any (`elem` (g A.! v)) (S.toList comp)
        , v `S.notMember` comp
        ]
    computeRecursionIdents :: S.Set Ident -> M.Map Ident [Expr]
    computeRecursionIdents comp = todo

-- computeRecursionIdents :: S.Set Ident -> M.Map Ident [Expr]
-- computeRecursionIdents = undefined

callClosure :: (Ident -> Function) -> S.Set Ident -> S.Set Ident
callClosure lookupFunction = go S.empty
  where
    go visited toVisit = case S.minView toVisit of
        Nothing -> visited
        Just (cur, rest) ->
            let neighbors = lookupFunction cur
                    ^.. #body % _Just % #nodes % folded % #_NodeCall % #functionName
                in go (S.insert cur visited) (rest <> S.fromList neighbors)
