{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.Core.GraphSlice.New.AsmRefine
    ( areMemCallsCompatible
    , asmRefineIsStackHook
    ) where

import BV.Core.GraphSlice.New.MemCalls

import BV.Core.Types
import BV.Core.Types.Extras

import Data.List (genericIndex)
import qualified Data.Map as M
import qualified Data.Set as S

areMemCallsCompatible
    :: RefineTag t
    => (WithTag t Ident -> PairingId t)
    -> ByTag t MemCalls
    -> Bool
areMemCallsCompatible lookupPairingId calls =
    let cCastCalls = M.fromList
            [ let pairingId = lookupPairingId (WithTag leftTag asmFunName)
               in (getRight pairingId, asmCallsForFun)
            | (asmFunName, asmCallsForFun) <- M.toList (getLeft calls)
            ]
        compat cFunName =
            let cCast = M.findWithDefault zeroMemCallsRange cFunName cCastCalls
                cActual = M.findWithDefault zeroMemCallsRange cFunName (getRight calls)
             in memCallsRangesOverlap cCast cActual
     in all compat $ S.toList $ M.keysSet (getRight calls) <> M.keysSet cCastCalls

asmRefineIsStackHook :: HasTagIsAsm t => LookupFunctionSignature t -> WithTag t Ident -> FunctionSignatureDirection -> Integer -> Bool
asmRefineIsStackHook lookupSig fun direction i =
    tagIsAsm fun.tag && genericIndex (viewFunctionSignatureDirection direction (lookupSig fun)) i == asmStackVar

asmStackVar :: NameTy
asmStackVar = NameTy (Ident "stack") memT
