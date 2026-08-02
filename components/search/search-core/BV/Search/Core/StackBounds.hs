{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.Search.Core.StackBounds
    ( DiscoverStackBoundsInput (..)
    , discoverStackBounds
    ) where

import BV.Search.Core.StackBounds.Base
import BV.Search.Core.StackBounds.C

import BV.Search.Core.GraphSlice
import BV.Search.Core.Solver

import BV.Core.Stages
import BV.Core.Types
import BV.Core.Types.Extras.Expr (andE, eqE, falseE, ifThenElseE, minusE, notE,
                                  trueE, varE, word32E, word32T)
import BV.Core.Types.Extras.Problem
import BV.Core.Types.Extras.Program (signatureOfFunction)
import BV.Core.Types.Extras.ProofCheck (eqH, eqSideH, pcFalseH)
import BV.Logging
import BV.Utils (ensure, ensureM, expecting)

import Control.DeepSeq (force)
import Control.Monad (guard, unless, when)
import Control.Monad.Extra (concatForM, mapMaybeM)
import Control.Monad.State (StateT, evalStateT, gets)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Foldable (for_)
import qualified Data.Graph as G
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S
import Data.Traversable (for)
import Optics
import Text.Printf (printf)

type AsmTag = ConstAsmRefineTag 'Asm

asmTag :: AsmTag
asmTag = ConstAsmRefineTag

withAsmTag :: a -> WithTag AsmTag a
withAsmTag = WithTag asmTag

discoverStackBounds
    :: forall m n. (Monad m, MonadGraphSliceGetSExprValue n, MonadLoggerWithContext m, MonadLoggerWithContext n)
    => (forall a. n a -> m a)
    -> (forall a b t. Traversable t => t a -> (a -> m b) -> m (t b))
    -> DiscoverStackBoundsInput
    -> m StackBounds
discoverStackBounds run forConcurrently input = do

    cIdents <- getRecursionIdents runGraphSliceC $ M.fromSet lookupCFun cClosure
    let asmIdents = convertRecursionIdents cIdents
    logInfo "Computed recursion limits."

    immed <- computeImmediateStackBounds asmIdents
    let bounds = computeRecursiveStackBounds immed
    logInfo "Computed stack bounds."

    let closedForm fname =
            let go (ident:rest) =
                    let bound = word32E (bounds M.! (fname, ident))
                    in case rest of
                            [] -> ensure (ident == trueE) $ bound
                            _ -> ifThenElseE ident bound (go rest)
            in go $ M.findWithDefault [trueE] fname asmIdents

    return $ StackBounds $ flip M.fromSet asmClosure closedForm

  where

    lookupAsmFun = input.lookupFunction . WithTag Asm
    lookupCFun = input.lookupFunction . WithTag C

    asmClosure = callClosure lookupAsmFun input.includeAsmFrom
    cClosure = callClosure lookupCFun $
        S.fromList $ mapMaybe (flip M.lookup asmToC) (S.toList asmClosure)

    cToAsm = M.fromList [ (pid.c, pid.asm) | pid <- S.toList input.pairingIds ]
    asmToC = M.fromList [ (pid.asm, pid.c) | pid <- S.toList input.pairingIds ]

    asmFunIsPaired = (`S.member` S.map (.asm) input.pairingIds)

    runGraphSliceC :: forall t a. HasTagIsAsm t => ProblemWithAnalysis t -> GraphSliceT t n a -> m a
    runGraphSliceC p m = run $ runGraphSlice input C p m

    runGraphSliceAsm :: forall t a. HasTagIsAsm t => ProblemWithAnalysis t -> GraphSliceT t n a -> m a
    runGraphSliceAsm p m = run $ runGraphSlice input Asm p m

    convertRecursionIdents cIdents = M.fromList
        [ let sig = signatureOfFunction (lookupCFun cFunName)
           in (asmFunName, map (convertIdent sig) cFunIdents)
        | (cFunName, cFunIdents) <- M.toList cIdents
        , Just asmFunName <- return $ M.lookup cFunName cToAsm
        ]

    computeImmediateStackBounds
        :: M.Map Ident [GraphExpr] -> m (M.Map StackBoundKey (Integer, M.Map StackBoundKey Integer))
    computeImmediateStackBounds asmIdents = fmap M.unions $ forConcurrently (S.toAscList asmClosure) $ \callerName -> do
        logInfo $ printf "  stack analysis for %P" callerName
        (maxOffset, maxOffsetByCallee, callables) <- fmap (fromMaybe (0, M.empty, M.empty)) $ runMaybeT $ do
            p <- buildProb callerName
            (maxOffset, maxOffsetByCallee) <- lift $ guessAsmStackDepth callerName p
            callables <- lift $ identCallables asmIdents callerName p (M.keys maxOffsetByCallee)
            return (maxOffset, maxOffsetByCallee, callables)
        return $! force $ M.fromList
            [ let calls = M.fromList
                    [ ((calleeName, calleeIdent), calleeOffset)
                    | (calleeName, calleeOffset) <- M.toList maxOffsetByCallee
                    , calleeIdent <- M.findWithDefault [trueE] calleeName asmIdents
                    , M.findWithDefault False (callerIdent, calleeName, calleeIdent) callables
                    ]
                    in ((callerName, callerIdent), (maxOffset, calls))
            | callerIdent <- M.findWithDefault [trueE] callerName asmIdents
            ]

    buildProb :: Ident -> MaybeT m (ProblemWithAnalysis AsmTag)
    buildProb funName = flip evalStateT initProblemBuilder $ do
        let fun = lookupAsmFun funName
        lift $ guard $ isJust fun.body
        addEntrypoint $ withAsmTag $ Named funName fun
        doAnalysis
        patchNoreturnCallConts asmFunIsNoreturn
        doAnalysis
        intermed <- gets $ problemSideWithAnalysis asmTag . extractProblemWithAnalysis
        lift $ guard $ all loopIsSimple intermed.analysis.loopData.outermostLoops
        inlineNoPrePairing asmFunIsPaired lookupAsmFun
        doAnalysis
        gets extractProblemWithAnalysis

    guessAsmStackDepth :: Ident -> ProblemWithAnalysis AsmTag -> m (Integer, M.Map Ident Integer)
    guessAsmStackDepth funName p = do
        let side = problemSideWithAnalysis asmTag p
        let argRenames = problemArgRenames p.problem $ byTagFrom $ const $ signatureOfFunction $ lookupAsmFun funName
        let spName = argRenames (PairingEqSideQuadrant asmTag PairingEqDirectionIn) (Ident "r13")
        let spVar = varE word32T spName
        let visitOf = defaultVisit side.analysis.loopData asmTag . Addr
        let spPreservationHyps =
                [ let f = eqSideH spVar . visitOf
                   in f addr `eqH` f cont
                | (addr, NodeCall (CallNode { next = Addr cont })) <- M.toList side.problem.nodes
                ]
        spOffsets <- runGraphSliceAsm p $ do
            PcEnv _ entryEnv <- fromJust <$> getPcEnv (Visit asmTag side.problem.entryPoint M.empty)
            let spInit = flattenExpr entryEnv spVar
            symbolicOffsets <- flip mapMaybeM (M.keys side.problem.nodes) $ \addr -> do
                opt <- getPcEnv (visitOf addr)
                return $ opt <&> \(PcEnv _ env) ->
                    (addr, flattenExpr env spVar `minusE` spInit)
            testHypWhyps falseE spPreservationHyps >>= ensureM . not
            concreteOffsets <- for symbolicOffsets $ \(addr, symbolicOffset) -> do
                val <- getFlatExprValue symbolicOffset
                return (addr, symbolicOffset, val)
            for concreteOffsets $ \(addr, symbolicOffset, concreteOffset) -> do
                stable <- testHypWhyps (symbolicOffset `eqE` concreteOffset) spPreservationHyps
                ensureM stable
                return (addr, signedOffset (numValOf concreteOffset))
        let (maxOffset, sign) =
                let spOffsetVals = map snd spOffsets
                    min_ = minimum spOffsetVals
                    max_ = maximum spOffsetVals
                    in if min_ < 0 then (negate min_, -1) else (max_, 1)
        let maxOffsetByCallee = M.fromListWith max
                [ (callNode.functionName, offset * sign)
                | (addr, offset) <- spOffsets
                , NodeCall callNode <- return $ side.problem.nodes M.! addr
                ]
        return (maxOffset, maxOffsetByCallee)

    -- Which (caller-ident, callee, callee-ident) triples are reachable. Callees without identifiers
    -- are always callable. For callees with identifiers we test, per caller-ident, whether the call
    -- can happen with the callee-ident's condition.
    identCallables
        :: M.Map Ident [GraphExpr] -> Ident -> ProblemWithAnalysis AsmTag -> [Ident] -> m (M.Map (GraphExpr, Ident, GraphExpr) Bool)
    identCallables asmIdents callerName p calleeNames = do
        let callerIdents = M.findWithDefault [trueE] callerName asmIdents
        -- The immediate-bounds graph can only cycle among recursion-group members, so only those
        -- (idented functions calling idented callees) need the SMT callability test to break the
        -- cycle. Non-group callers keep every edge.
        if not (callerName `M.member` asmIdents && any (`M.member` asmIdents) calleeNames)
            then return $ M.fromList
                [ ((callerIdent, calleeName, calleeIdent), True)
                | callerIdent <- callerIdents
                , calleeName <- calleeNames
                , calleeIdent <- M.findWithDefault [trueE] calleeName asmIdents
                ]
            else do
                let auto = M.fromList
                        [ ((callerIdent, calleeName, trueE), True)
                        | callerIdent <- callerIdents
                        , calleeName <- calleeNames
                        , calleeName `M.notMember` asmIdents
                        ]
                let side = problemSideWithAnalysis asmTag p
                let entryVis = Visit asmTag side.problem.entryPoint M.empty
                let visitOf = defaultVisit side.analysis.loopData asmTag
                let toCheck =
                        [ (callsiteAddr, callsiteNode, calleeIdent, calleeIdentCond)
                        | (callsiteAddr, NodeCall callsiteNode) <- M.toAscList side.problem.nodes
                        , Just calleeIdents <- return $ M.lookup callsiteNode.functionName asmIdents
                        , (calleeIdent, calleeIdentCond) <- identConds calleeIdents
                        ]
                results <- runGraphSliceAsm p $ do
                    concatForM (identConds callerIdents) $ \(callerIdent, callerIdentCond) -> do
                        for toCheck $ \(callsiteAddr, callsiteNode, calleeIdent, calleeIdentCond) -> do
                            possible <- getPcEnv (visitOf (Addr callsiteAddr)) >>= \case
                                Nothing -> return False
                                Just (PcEnv pc env) -> do
                                    let calleeInputs = (lookupAsmFun callsiteNode.functionName).input
                                    let inpEnv = M.fromList $ zip
                                            (map (.name) calleeInputs)
                                            (map (flattenExpr env) callsiteNode.input)
                                    let new = pc `andE` flattenExpr inpEnv calleeIdentCond
                                    -- Note that graph-refine adds mk_not_callable_hyps here
                                    covered <- testHypWhyps (notE new)
                                        [ pcFalseH (visitOf Err)
                                        , eqSideH callerIdentCond entryVis
                                            `eqH` eqSideH trueE entryVis
                                        ]
                                    return $ not covered
                            return ((callerIdent, callsiteNode.functionName, calleeIdent), possible)
                return $ M.unionWith (||) (M.fromList results) auto


-- HACK
asmFunIsNoreturn :: Ident -> Bool
asmFunIsNoreturn =
    (`elem`
        [ "halt"
        ])

-- Repeatedly inline calls to functions with extant bodies which are not paired with C functions.
-- Note that graph-refine uses callee names to determine whether they are instruction functions
-- rather in place of this more general extant body check.
inlineNoPrePairing
    :: Monad m
    => (Ident -> Bool)
    -> (Ident -> Function)
    -> StateT (ProblemBuilder AsmTag) m ()
inlineNoPrePairing isPaired lookupAsmFun =
    go (0 :: Int)
  where
    go iters = do
        when (iters > 10000) $ error "inlineNoPrePairing: iteration cap exceeded"
        p <- gets extractProblem
        let side = viewAtTag asmTag p.sides
        let toInline =
                [ WithTag asmTag addr
                | (addr, NodeCall callNode) <- M.toAscList side.nodes
                , not (isPaired callNode.functionName)
                , isJust (lookupAsmFun callNode.functionName).body
                ]
        unless (null toInline) $ do
            for_ toInline $ inlineAtPoint $ \f -> lookupAsmFun f.value
            doAnalysis
            go (iters + 1)

-- Interpret a 32-bit value as a signed offset.
signedOffset :: Integer -> Integer
signedOffset c = if m >= b31 then m - b32 else m
  where
    b31 = 2 ^ (31 :: Int)
    b32 = 2 ^ (32 :: Int)
    m = c `mod` b32

numValOf :: Expr c -> Integer
numValOf e = e.value ^. expecting #_ExprValueNum

-- Pair each recursion identifier with its cumulative condition (this identifier holds and all
-- earlier ones did not).
identConds :: [GraphExpr] -> [(GraphExpr, GraphExpr)]
identConds = go trueE
  where
    go _ [] = []
    go rolling (i : is) = (i, andE rolling i) : go (andE rolling (notE i)) is

type StackBoundKey = (Ident, GraphExpr)

computeRecursiveStackBounds
    :: M.Map StackBoundKey (Integer, M.Map StackBoundKey Integer)
    -> M.Map StackBoundKey Integer
computeRecursiveStackBounds immed
    -- The per-(function,identifier) call graph must be acyclic. If it is cyclic (e.g. callability
    -- was over-approximated), the lazy resolution below would diverge, so fail fast with a clear
    -- message instead.
    | not (null cyclic) = error $
        "computeRecursiveStackBounds: immediate-bounds graph has a cycle (recursion identifiers did not break it): "
            ++ show
                [ [ ident.unwrap | (ident, _) <- k ]
                | k <- cyclic
                ]
    | otherwise = bounds
  where
    cyclic = [ ks | G.CyclicSCC ks <- G.stronglyConnComp graphEdges ]
    graphEdges = [ (k, k, M.keys calls) | (k, (_, calls)) <- M.toList immed ]
    bounds = M.map resolve immed
    resolve (static, calls) = maximum $
        static : [ bounds M.! k + off | (k, off) <- M.toList calls ]
