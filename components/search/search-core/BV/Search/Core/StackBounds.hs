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
import BV.Core.Types.Extras.Problem
import BV.Logging
import BV.Search.Core.Solver
import BV.Utils

import Control.DeepSeq (NFData)
import Control.Monad.State (StateT, execStateT, get, gets, modify)
import Control.Monad.Trans (lift)
import qualified Data.Array as A
import Data.Foldable (for_, toList)
import qualified Data.Graph as G
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics
import Text.Printf (PrintfArg (formatArg), printf)

-- TODO WIP WIP WIP

data DiscoverStackBoundsInput
  = DiscoverStackBoundsInput
      { structs :: ByTag' (M.Map Ident Struct)
      , rodata :: ROData
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
    logInfo ""
    let asmClosure = callClosure (input.functions . WithTag Asm) input.includeAsmFrom
    -- logInfo $ show ("asm closure", asmClosure ^.. folded % #unwrap)
    let cClosure = S.fromList $ input.pairings ^.. folded % filtered ((`S.member` asmClosure) . (.asm)) % atTag C
    -- logInfo $ show ("c closure", cClosure ^.. folded % #unwrap)
    cIdents <- getRecursionIdents
        runRepGraphAsm
        (M.fromSet (input.functions . WithTag C) cClosure)
    for_ (M.toList cIdents) $ \(k, v) -> logInfo $ show ("c ident", k.unwrap, v)
    todo

  where

    runRepGraph :: forall t a. Tag t => AsmRefineTag -> Problem t -> RepGraphBase t n a -> m a
    runRepGraph tag problem m =
        let repGraphInput = RepGraphBaseInput
                { structs = viewAtTag tag input.structs <$ problem.sides
                , rodata = input.rodata
                , problem
                }
            in run (runRepGraphBase repGraphInput m)

    runRepGraphC :: forall t a. Tag t => Problem t -> RepGraphBase t n a -> m a
    runRepGraphC = runRepGraph C

    runRepGraphAsm :: forall t a. Tag t => Problem t -> RepGraphBase t n a -> m a
    runRepGraphAsm = runRepGraph Asm

getRecursionIdents
    :: forall m n. (Monad m, MonadRepGraphSolverInteract n, MonadLoggerWithContext m, MonadLoggerWithContext n)
    => (forall t a. Tag t => Problem t -> RepGraphBase t n a -> m a)
    -> M.Map Ident Function
    -> m (M.Map Ident [Expr])
getRecursionIdents runRepGraph functions =
    mconcat <$> traverse (computeRecursionIdents . S.map fromVertex) scc
  where
    (g, fromVertex', toVertex') = G.graphFromEdges
        [ ((), n, functionCalls fun)
        | (n, fun) <- M.toList functions
        ]
    fromVertex = view _2 . fromVertex'
    toVertex = fromJust . toVertex'
    scc =
        [ comp
        | comp <- map (S.fromList . toList) (G.scc g)
        , S.size comp > 1 || callsSelf (S.findMin comp)
        ]
    callsSelf v = v `elem` (g A.! v)
    prevs comp' = S.fromList
        [ fromVertex v
        | v <- G.vertices g
        , any (`elem` (g A.! v)) (S.toList comp)
        , v `S.notMember` comp
        ]
      where
        comp = S.map toVertex comp'
    computeRecursionIdents :: S.Set Ident -> m (M.Map Ident [Expr])
    computeRecursionIdents group = flip execStateT M.empty $ do
        logInfo $ printf "Doing recursion analysis for function group:"
        logInfo $ printf "  %s" $ show $ map (.unwrap) $ S.toList group
        for_ (S.toList (S.difference (prevs group) group)) $ \f -> do
            logInfo $ printf "  checking for for %P" f
            whileM (addRecursionIdent runRepGraph functions f group) (return ())

addRecursionIdent
    :: forall m n. (Monad m, MonadRepGraphSolverInteract n, MonadLoggerWithContext m, MonadLoggerWithContext n)
    => (forall t a. Tag t => Problem t -> RepGraphBase t n a -> m a)
    -> M.Map Ident Function
    -> Ident
    -> S.Set Ident
    -> StateT (M.Map Ident [Expr]) m Bool
addRecursionIdent runRepGraph functions f group = do
    let initState = (initProblemBuilder, [], [])
    flip execStateT initState $ do
        let mostRecentTag = FunTag . length . (.sides) <$> zoom _1 (gets extractProblem)
        zoom _1 $ do
            addEntrypoint (WithTag (FunTag 0) (Named f (functions M.! f)))
            doAnalysis
        let go = do
                pa <- zoom _1 $ gets extractProblemWithAnalysis
                idents <- lift get
                assns <- zoom _3 get
                tag <- mostRecentTag
                resOpt <- lift $ lift $ findUnknownRecursion runRepGraph functions pa.problem group idents tag assns
                for_ resOpt $ \res -> do
                    fname <- zoom _1 $ use $ to extractProblem % #nodes % at res % unwrapped % expecting #_NodeCall % #functionName
                    zoom _2 $ modify (++ [fname])
                    len <- zoom _2 $ gets length
                    let nextTag = FunTag len
                    zoom _1 $ addEntrypoint $ WithTag nextTag $ Named fname $ functions M.! fname
                    zoom _1 doAnalysis
                    todo
                todo
        go
    todo

-- defaultVisit :: Tag t => Problem t -> NodeAddr -> Visit
-- defaultVisit p n =
--     todo
--   where
--     m = makeLoopData p
--     h = loopHeadOf n m

findUnknownRecursion
    :: forall m n. (Monad m, MonadRepGraphSolverInteract n, MonadLoggerWithContext m, MonadLoggerWithContext n)
    => (forall t a. Tag t => Problem t -> RepGraphBase t n a -> m a)
    -> M.Map Ident Function
    -> Problem FunTag
    -> S.Set Ident
    -> M.Map Ident [Expr]
    -> FunTag
    -> [Hyp FunTag]
    -> m (Maybe NodeAddr)
findUnknownRecursion runRepGraph group idents tag assns = undefined

--

newtype FunTag
  = FunTag Int
  deriving (Enum, Eq, Generic, NFData, Ord, Show)

instance PrintfArg FunTag where
    formatArg = formatArgSimple prettyTag

instance Tag FunTag where
    prettyTag (FunTag i) = "fun" ++ show i
    parsePrettyTag = unimplemented

functionCalls :: Function -> [Ident]
functionCalls = toListOf $ #body % _Just % #nodes % folded % #_NodeCall % #functionName

callClosure :: (Ident -> Function) -> S.Set Ident -> S.Set Ident
callClosure lookupFunction = go S.empty
  where
    go visited toVisit = case S.minView toVisit of
        Nothing -> visited
        Just (cur, rest) ->
            let neighbors = lookupFunction cur
                    ^.. #body % _Just % #nodes % folded % #_NodeCall % #functionName
                visited' = S.insert cur visited
             in go visited' (rest <> S.difference (S.fromList neighbors) visited')
