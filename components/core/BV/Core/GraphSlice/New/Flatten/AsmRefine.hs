{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.Core.GraphSlice.New.Flatten.AsmRefine
    ( areMemCallsCompatible
    , asmRefineIsMemHook
    , asmRefineIsStackHook
    , asmRefineStackPointerHook
    ) where

import BV.Core.GraphSlice.New.Flatten.MemCalls

import BV.Core.Types
import BV.Core.Types.Extras

import Data.List (genericIndex)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Optics

areMemCallsCompatible
    :: t ~ AsmRefineTag
    => LookupFunctionSignature t
    -> (WithTag t Ident -> PairingId t)
    -> ByTag t (Maybe MemCalls) -> Bool
areMemCallsCompatible lookupSig lookupPairingId callsOpt = case sequenceA callsOpt of
    Nothing -> True
    Just calls ->
        let hasMem cFunName =
                any
                    (asmRefineIsMemHook lookupSig (WithTag C cFunName) PairingEqDirectionIn)
                    (zipWith const [0..] (lookupSig (WithTag C cFunName)).output)
            cCastCalls = M.fromList $ catMaybes
                [ let pairingId = lookupPairingId (WithTag Asm asmFunName)
                      cFunName = pairingId.c
                   in if hasMem cFunName
                      then Just (cFunName, asmCallsForFun)
                      else Nothing
                | (asmFunName, asmCallsForFun) <- M.toList calls.asm
                ]
            compat rname =
                let rcast = M.findWithDefault zeroMemCallsRange rname cCastCalls
                    ractual = M.findWithDefault zeroMemCallsRange rname calls.c
                 in memCallsRangesOverlap rcast ractual
         in all compat $ S.toList $ M.keysSet calls.c <> M.keysSet cCastCalls

asmRefineIsMemHook :: t ~ AsmRefineTag => LookupFunctionSignature t -> WithTag t Ident -> PairingEqDirection -> Integer -> Bool
asmRefineIsMemHook lookupSig fun direction i =
    genericIndex (view (directionSigLabel direction) sig) i == NameTy (Ident memName) memT
  where
    sig = lookupSig fun
    isInstFun = any (== NameTy (Ident "inst_ident") tokenT) sig.input -- HACK
    memName = case fun.tag of
        Asm -> "mem"
        C | isInstFun -> "mem"
        C -> "Mem"

asmRefineIsStackHook :: t ~ AsmRefineTag => LookupFunctionSignature t -> WithTag t Ident -> PairingEqDirection -> Integer -> Bool
asmRefineIsStackHook lookupSig fun direction i =
    fun.tag == Asm && genericIndex (view (directionSigLabel direction) (lookupSig fun)) i == asmStackVar

directionSigLabel :: PairingEqDirection -> Lens' FunctionSignature [NameTy]
directionSigLabel = \case
    PairingEqDirectionIn -> #input
    PairingEqDirectionOut -> #output

asmStackVar :: NameTy
asmStackVar = NameTy (Ident "stack") memT

asmRefineStackPointerHook :: ArgRenames AsmRefineTag -> AsmRefineTag -> GraphExpr
asmRefineStackPointerHook argRenames tag = case tag of
    Asm -> varE machineWordT $
        argRenames
        (PairingEqSideQuadrant tag PairingEqDirectionIn)
        (Ident "r13")
