{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Stages.FormulatePairing
    ( formulatePairing
    ) where

import BV.Core.Logic (splitScalarPairs)
import BV.Core.Types
import BV.Core.Types.Extras

import Data.Maybe (mapMaybe, maybeToList)
import Optics

formulatePairing :: Expr -> FunctionSignature -> Pairing'
formulatePairing minStackSize sig = Pairing { inEqs, outEqs }
  where
    (varArgsC, imemC, _globArgsC) = splitScalarPairs sig.input
    (varRetsC, omemC, _globRetsC) = splitScalarPairs sig.output

    r i = machineWordVarE (Ident ("r" ++ show (i :: Integer)))
    sp = r 13
    stack = varE memT "stack"
    r0Input = machineWordVarE "ret_addr_input"
    asmMem = varE memT "mem"
    ret = machineWordVarE "ret"

    preconds =
        [ alignedE 2 sp
        , ret `eqE` r 14
        , alignedE 2 ret
        , r0Input `eqE` r 0
        , minStackSize `lessEqE` sp
        ]
        ++
        retPreconds
        ++
        maybeToList (lessEqE sp <$> outerAddr)

    postEqs =
        [ (r i, r i)
        | i <- [4..11] ++ [13]
        ]
        ++
        retPostEqs
        ++
        [ (stackWrapperE sp stack argSeqAddrs, stackWrapperE sp stack saveAddrs)
        ]

    multiRet = length varRetsC > 1

    firstArgIndex = if multiRet then 1 else 0

    argSeq =
        [ (r i, Nothing)
        | i <- [firstArgIndex..3]
        ]
        ++
        [ (value, Just addr)
        | (value, addr) <- mkStackSequence sp 4 stack machineWordT (length varArgsC + 1)
        ]

    (retPreconds, retPostEqs, retOutEqs, saveAddrs) =
        if not multiRet
        then
            let theseRetOutEqs = zip (map varFromNameTyE varRetsC) [r 0]
             in ([], [], theseRetOutEqs, [])
        else
            let theseRetPreconds =
                    [ alignedE 2 (r 0)
                    , sp `lessEqE` r 0
                    ] ++
                    maybeToList (lastOf folded initSaveSeq <&> \(_, addr) ->
                        r 0 `lessEqE` addr) ++
                    concat (maybeToList (lastArgAddr <&> \lastArgAddr' ->
                        [ lastArgAddr' `lessE` addr | (_, addr) <- take 1 initSaveSeq ]))
                saveSeq = mkStackSequence r0Input 4 stack machineWordT (length varRetsC)
                theseSaveAddrs = map snd saveSeq
                theseRetPostEqs = [(r0Input, r0Input)]
                theseRetOutEqs =
                    [ (varFromNameTyE c, castE c.ty a)
                    | (c, (a, _)) <- zip varRetsC saveSeq
                    ]
                initSaveSeq = mkStackSequence (r 0) 4 stack machineWordT (length varRetsC)
                lastArgAddr = snd $ case length varArgsC of
                    0 -> last argSeq
                    n -> argSeq !! (n - 1)
             in (theseRetPreconds, theseRetPostEqs, theseRetOutEqs, theseSaveAddrs)

    argSeqAddrs = mapMaybe snd (take (length varArgsC) argSeq)

    memIeqs = case imemC of
        [] -> [leftIn (rodataE asmMem) === rightIn trueE]
        [imemC'] ->
            [ leftIn asmMem === rightIn (varFromNameTyE imemC')
            , rightIn (rodataE (varFromNameTyE imemC')) === rightIn trueE
            ]

    memOeqs = case omemC of
        [] -> [leftOut asmMem === leftIn asmMem]
        [omemC'] ->
            [ leftOut asmMem === rightOut (varFromNameTyE omemC')
            , rightOut (rodataE (varFromNameTyE omemC')) === rightOut trueE
            ]

    outerAddr = lastOf folded (take (length varArgsC) argSeq) >>= snd

    argEqs =
        [ leftIn asm === rightIn (castCToAsmE asm.ty (varFromNameTyE c))
        | (c, (asm, _addr)) <- zip varArgsC argSeq
        ]

    inEqs = argEqs ++ memIeqs ++ [ leftIn expr === leftIn trueE | expr <- preconds ]

    retEqs = [ leftOut asm === rightOut (castCToAsmE asm.ty c) | (c, asm) <- retOutEqs ]

    leftInvs = [ leftIn vin === leftOut vout | (vin, vout) <- postEqs ]

    outEqs = retEqs ++ memOeqs ++ leftInvs

mkStackSequence :: Expr -> Integer -> Expr -> ExprType -> Int -> [(Expr, Expr)]
mkStackSequence sp offs stack ty n =
    [ let addr = sp `plusE` numE sp.ty (offs * i)
          expr = memAccE ty addr stack
       in (expr, addr)
    | i <- take n [0..]
    ]
