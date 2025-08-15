{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Stages.FormulatePairing
    ( formulatePairing
    ) where

import BV.Core.Arch (archPtrSizeBytes)
import BV.Core.Logic (splitScalarPairs)
import BV.Core.Types
import BV.Core.Types.Extras

import Data.Foldable (toList)
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
        | (value, addr) <- take (length varArgsC + 1) $ mkStackSequence sp stack
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
                theseRetPostEqs = [(r0Input, r0Input)]
                (theseRetOutEqs, theseSaveAddrs) = unzip
                    [ ((varFromNameTyE c, castE c.ty a), addr)
                    | (c, (a, addr)) <- zip varRetsC $ mkStackSequence r0Input stack
                    ]
                initSaveSeq = take (length varRetsC) $ mkStackSequence (r 0) stack
                lastArgAddr = snd $ case length varArgsC of
                    0 -> last argSeq
                    n -> argSeq !! (n - 1)
             in (theseRetPreconds, theseRetPostEqs, theseRetOutEqs, theseSaveAddrs)

    argSeqAddrs = mapMaybe snd (take (length varArgsC) argSeq)

    (memIeqs, memOeqs) =
        mkMemEqs
            asmMem
            (maybeFromList (map varFromNameTyE imemC))
            (Just asmMem)
            (maybeFromList (map varFromNameTyE omemC))

    outerAddr = lastOf folded (take (length varArgsC) argSeq) >>= snd

    argEqs =
        [ asmIn asm === cIn (castCToAsmE asm.ty (varFromNameTyE c))
        | (c, (asm, _addr)) <- zip varArgsC argSeq
        ]

    inEqs = argEqs ++ memIeqs ++ [ asmIn expr === asmIn trueE | expr <- preconds ]

    retEqs = [ asmOut asm === cOut (castCToAsmE asm.ty c) | (c, asm) <- retOutEqs ]

    leftInvs = [ asmIn vin === asmOut vout | (vin, vout) <- postEqs ]

    outEqs = retEqs ++ memOeqs ++ leftInvs

mkStackSequence :: Expr -> Expr -> [(Expr, Expr)]
mkStackSequence sp stack =
    [ let addr = sp `plusE` numE sp.ty (archPtrSizeBytes * i)
          expr = memAccE machineWordT addr stack
       in (expr, addr)
    | i <- [0..]
    ]

mkMemEqs :: Expr -> Maybe Expr -> Maybe Expr -> Maybe Expr -> ([PairingEq AsmRefineTag], [PairingEq AsmRefineTag])
mkMemEqs asmInMem cInMemOpt asmOutMemOpt cOutMemOpt =
    (inEqs, outEqs)
  where
    inEqs = case cInMemOpt of
        Nothing ->
            [ asmIn (rodataE asmInMem) === cIn trueE
            ]
        Just cInMem ->
            [ asmIn asmInMem === cIn cInMem
            , cIn (rodataE cInMem) === cIn trueE
            ]
    outEqs = case (asmOutMemOpt, cOutMemOpt) of
        (_, Nothing) ->
            [ asmOut asmOutMem === asmIn asmInMem
            | asmOutMem <- toList asmOutMemOpt
            ]
        (Just asmOutMem, Just cOutMem) ->
            [ asmOut asmOutMem === cOut cOutMem
            , cOut (rodataE cOutMem) === cOut trueE
            ]

maybeFromList :: [a] -> Maybe a
maybeFromList = \case
    [] -> Nothing
    [a] -> Just a
