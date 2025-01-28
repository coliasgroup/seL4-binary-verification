{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-# HLINT ignore "Use head" #-}

module BV.Core.Stages.FormulatePairing
    ( formulatePairing
    ) where

import Data.Functor ((<&>))
import Data.List (partition)
import Data.Maybe (fromJust, mapMaybe, maybeToList)

import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils

formulatePairing :: Expr -> [Argument] -> [Argument] -> Pairing
formulatePairing minStackSize inputC outputC = Pairing { inEqs, outEqs }
  where
    (varArgsC, imemC, _globArgsC) = splitScalarPairs inputC
    (varRetsC, omemC, _globRetsC) = splitScalarPairs outputC

    r i = machineWordVarE (Ident ("r" ++ show (i :: Integer)))
    stackPointer = r 13
    stack = varE memT "stack"
    r0Input = machineWordVarE "ret_addr_input"
    asmMem = varE memT "mem"
    ret = machineWordVarE "ret"

    preconds =
        [ alignedE 2 stackPointer
        , ret `eqE` r 14
        , alignedE 2 ret
        , r0Input `eqE` r 0
        , minStackSize `lessEqE` stackPointer
        ] ++
        retPreconds
        ++
        maybeToList (outerAddr <&> \addr -> stackPointer `lessEqE` addr)

    postEqs =
        [ (r i, r i)
        | i <- [4..11] ++ [13]
        ]
        ++
        retPostEqs
        ++
        [(stackWrapperE stackPointer stack argSeqAddrs, stackWrapperE stackPointer stack saveAddrs)]

    firstArgIndex = if multiRet then 1 else 0

    argSeq =
        [ (r i, Nothing)
        | i <- [firstArgIndex..3]
        ] ++
        mkStackSequence stackPointer 4 stack machineWordT (length varArgsC + 1)

    multiRet = length varRetsC > 1

    (retPreconds, retPostEqs, retOutEqs, saveAddrs) =
        if not multiRet
        then
            let theseRetOutEqs = zip (map varFromArgE varRetsC) [r 0]
             in ([], [], theseRetOutEqs, [])
        else
            let
                theseRetPreconds =
                    [ alignedE 2 (r 0)
                    , stackPointer `lessEqE` r 0
                    ] ++
                    maybeToList (tryLast initSaveSeq <&> \(_, addr) ->
                        r 0 `lessEqE` fromJust addr) ++
                    concat (maybeToList (lastArgAddr <&> \lastArgAddr' ->
                        [ lastArgAddr' `lessE` fromJust addr | (_, addr) <- take 1 initSaveSeq ]))
                saveSeq = mkStackSequence r0Input 4 stack machineWordT (length varRetsC)
                theseSaveAddrs = saveSeq <&> \(_, Just addr) -> addr
                theseRetPostEqs = [(r0Input, r0Input)]
                theseRetOutEqs =
                    [ (varFromArgE c, castE c.ty a)
                    | (c, (a, _)) <- zip varRetsC saveSeq
                    ]
                initSaveSeq = mkStackSequence (r 0) 4 stack machineWordT (length varRetsC)
                lastArgAddr = snd $ case length varArgsC of
                    0 -> last argSeq
                    n -> argSeq !! (n - 1)
             in (theseRetPreconds, theseRetPostEqs, theseRetOutEqs, theseSaveAddrs)

    argSeqAddrs = mapMaybe snd (take (length varArgsC) argSeq)

    memIeqs = case imemC of
        [] -> [asmIn (rodataE asmMem) === cIn trueE]
        [imemC'] ->
            [ asmIn asmMem === cIn (varFromArgE imemC')
            , cIn (rodataE (varFromArgE imemC')) === cIn trueE
            ]

    memOeqs = case omemC of
        [] -> [asmOut asmMem === asmIn asmMem]
        [omemC'] ->
            [ asmOut asmMem === cOut (varFromArgE omemC')
            , cOut (rodataE (varFromArgE omemC')) === cOut trueE
            ]

    outerAddr = tryLast (take (length varArgsC) argSeq) >>= snd

    argEqs =
        [ asmIn asm === cIn (castCToAsmE asm.ty (varFromArgE c))
        | (c, (asm, _addr)) <- zip varArgsC argSeq
        ]

    inEqs =
        argEqs ++ memIeqs ++ [ asmIn expr === asmIn trueE | expr <- preconds ]

    retEqs = retOutEqs <&> \(c, asm) -> asmOut asm === cOut (castCToAsmE asm.ty c)

    asmInvs = postEqs <&> \(vin, vout) -> asmIn vin === asmOut vout

    outEqs =
        retEqs ++ memOeqs ++ asmInvs

-- TODO join with one in FormulatePairings
splitScalarPairs :: [Argument] -> ([Argument], [Argument], [Argument])
splitScalarPairs args = (scalars, mems, others)
  where
    (scalars, globals) = span (\arg -> isWordT arg.ty || isBoolT arg.ty) args
    (mems, others) = partition (\arg -> isMemT arg.ty) globals

mkStackSequence :: Expr -> Integer -> Expr -> ExprType -> Int -> [(Expr, Maybe Expr)]
mkStackSequence sp offs stack ty n =
    [ let addr = sp `plusE` numE sp.ty (offs * i)
          expr = memAccE ty addr stack
       in (expr, Just addr)
    | i <- take n [0..]
    ]
